/*
	This file is part of solidity.

	solidity is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	solidity is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with solidity.  If not, see <http://www.gnu.org/licenses/>.
*/
// SPDX-License-Identifier: GPL-3.0
#include <libsolidity/ast/AST.h>
#include <libsolidity/ast/ASTVisitor.h>
#include <libsolidity/interface/ReadFile.h>
#include <libsolidity/lsp/LanguageServer.h>
#include <libsolidity/lsp/ReferenceCollector.h>

#include <liblangutil/SourceReferenceExtractor.h>

#include <libsolutil/Visitor.h>
#include <libsolutil/JSON.h>

#include <boost/filesystem.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include <ostream>

#include <iostream>
#include <string>

using namespace std;
using namespace std::placeholders;

using namespace solidity::langutil;
using namespace solidity::frontend;

namespace solidity::lsp {

namespace // {{{ helpers
{
	// TODO: maybe use SimpleASTVisitor here, if that would be a simple free-fuunction :)
	class ASTNodeLocator : public ASTConstVisitor
	{
	private:
		int m_pos = -1;
		ASTNode const* m_currentNode = nullptr;

	public:
		explicit ASTNodeLocator(int _pos): m_pos{_pos}
		{
		}

		ASTNode const* closestMatch() const noexcept { return m_currentNode; }

		bool visitNode(ASTNode const& _node) override
		{
			if (_node.location().start <= m_pos && m_pos <= _node.location().end)
			{
				m_currentNode = &_node;
				return true;
			}
			return false;
		}
	};

	optional<string> extractPathFromFileURI(std::string const& _uri)
	{
		if (!boost::starts_with(_uri, "file://"))
			return nullopt;

		return _uri.substr(7);
	}

	string toFileURI(std::string const& _path)
	{
		return "file://" + _path;
	}

	void loadTextDocumentPosition(DocumentPosition& _params, Json::Value const& _json)
	{
		_params.path = extractPathFromFileURI(_json["textDocument"]["uri"].asString()).value();
		_params.position.line = _json["position"]["line"].asInt();
		_params.position.column = _json["position"]["character"].asInt();
	}
	Json::Value toJson(Range const& _range)
	{
		Json::Value json;
		json["start"]["line"] = _range.start.line;
		json["start"]["character"] = _range.start.column;
		json["end"]["line"] = _range.end.line;
		json["end"]["character"] = _range.end.column;
		return json;
	}
} // }}} end helpers

LanguageServer::LanguageServer(Transport& _client, Logger _logger):
	m_client{_client},
	m_handlers{
		{"cancelRequest", [](auto, auto) {/*don't do anything for now, as we're synchronous*/}},
		{"initialize", bind(&LanguageServer::handle_initialize, this, _1, _2)},
		{"initialized", {} },
		{"shutdown", [this](auto, auto) { m_shutdownRequested = true; }},
		{"workspace/didChangeConfiguration", bind(&LanguageServer::handle_workspace_didChangeConfiguration, this, _1, _2)},
		{"textDocument/didOpen", bind(&LanguageServer::handle_textDocument_didOpen, this, _1, _2)},
		{"textDocument/didChange", bind(&LanguageServer::handle_textDocument_didChange, this, _1, _2)},
		{"textDocument/didClose", [this](auto, Json::Value const& _args) {
			documentClosed(extractPathFromFileURI(_args["textDocument"]["uri"].asString()).value());
		}},
		{"textDocument/definition", bind(&LanguageServer::handle_textDocument_definition, this, _1, _2)},
		{"textDocument/documentHighlight", bind(&LanguageServer::handle_textDocument_highlight, this, _1, _2)},
		{"textDocument/references", bind(&LanguageServer::handle_textDocument_references, this, _1, _2)},
	},
	m_logger{std::move(_logger)},
	m_vfs()
{
}

void LanguageServer::changeConfiguration(Json::Value const& _settings)
{
	if (_settings["evm"].isString())
		if (auto const evmVersionOpt = EVMVersion::fromString(_settings["evm"].asString()); evmVersionOpt.has_value())
			m_evmVersion = evmVersionOpt.value();

	if (_settings["remapping"].isArray())
	{
		for (auto const& element: _settings["remapping"])
		{
			if (element.isString())
			{
				if (auto remappingOpt = CompilerStack::parseRemapping(element.asString()); remappingOpt.has_value())
					m_remappings.emplace_back(move(remappingOpt.value()));
				else
					trace("Failed to parse remapping: '"s + element.asString() + "'");
			}
		}
	}
}

void LanguageServer::documentContentUpdated(string const& _path, std::optional<int> _version, Range _range, std::string const& _text)
{
	// TODO: all this info is actually unrelated to solidity/lsp specifically except knowing that
	// the file has updated, so we can  abstract that away and only do the re-validation here.
	auto file = m_vfs.find(_path);
	if (!file)
	{
		log("LanguageServer: File to be modified not opened \"" + _path + "\"");
		return;
	}

	if (_version.has_value())
		file->setVersion(_version.value());

#if !defined(NDEBUG)
	ostringstream str;
	str << "did change: " << _range << " for '" << _text << "'";
	trace(str.str());
#endif
	file->modify(_range, _text);

}

void LanguageServer::documentContentUpdated(string const& _path, optional<int> _version, string const& _fullContentChange)
{
	auto file = m_vfs.find(_path);
	if (!file)
	{
		log("LanguageServer: File to be modified not opened \"" + _path + "\"");
		return;
	}

	if (_version.has_value())
		file->setVersion(_version.value());

	file->replace(_fullContentChange);

	validate(*file);
}

void LanguageServer::documentClosed(string const& _path)
{
	log("LanguageServer: didClose: " + _path);
}

void LanguageServer::validateAll()
{
	for (reference_wrapper<vfs::File const> const& file: m_vfs.files())
		validate(file.get());
}

frontend::ReadCallback::Result LanguageServer::readFile(string const& _kind, string const& _path)
{
	return m_fileReader->readFile(_kind, _path);
}

constexpr DiagnosticSeverity toDiagnosticSeverity(Error::Type _errorType)
{
	using Type = Error::Type;
	using Severity = DiagnosticSeverity;
	switch (_errorType)
	{
		case Type::CodeGenerationError:
		case Type::DeclarationError:
		case Type::DocstringParsingError:
		case Type::ParserError:
		case Type::SyntaxError:
		case Type::TypeError:
			return Severity::Error;
		case Type::Warning:
			return Severity::Warning;
	}
	// Should never be reached.
	return Severity::Error;
}

void LanguageServer::compile(vfs::File const& _file)
{
	// TODO: optimize! do not recompile if nothing has changed (file(s) not flagged dirty).

	// always start fresh when compiling
	m_sourceCodes.clear();

	m_sourceCodes[_file.path()] = _file.contentString();

	m_fileReader = make_unique<FileReader>(m_basePath, m_allowedDirectories);

	m_compilerStack.reset();
	m_compilerStack = make_unique<CompilerStack>(bind(&FileReader::readFile, ref(*m_fileReader), _1, _2));

	// TODO: configure all compiler flags like in CommandLineInterface (TODO: refactor to share logic!)
	OptimiserSettings settings = OptimiserSettings::standard(); // TODO: get from config
	m_compilerStack->setOptimiserSettings(settings);
	m_compilerStack->setParserErrorRecovery(false);
	m_compilerStack->setRevertStringBehaviour(RevertStrings::Default); // TODO get from config
	m_compilerStack->setSources(m_sourceCodes);
	m_compilerStack->setRemappings(m_remappings);

	m_compilerStack->setEVMVersion(m_evmVersion);

	trace("compile: using EVM "s + m_evmVersion.name());

	m_compilerStack->compile();
}

void LanguageServer::validate(vfs::File const& _file)
{
	compile(_file);

	std::vector<Diagnostic> collectedDiagnostics{};

	for (shared_ptr<Error const> const& error: m_compilerStack->errors())
	{
		// Don't show this warning. "This is a pre-release compiler version."
		if (error->errorId().error == 3805)
			continue;

		auto const message = SourceReferenceExtractor::extract(*error);

		auto const severity = toDiagnosticSeverity(error->type());

		// global warnings don't have positions in the source code - TODO: default them to top of file?

		auto const startPosition = LineColumn{{
			max(message.primary.position.line, 0),
			max(message.primary.startColumn, 0)
		}};

		auto const endPosition = LineColumn{{
			max(message.primary.position.line, 0),
			max(message.primary.endColumn, 0)
		}};

		Diagnostic diag{};
		diag.range.start.line = startPosition.line;
		diag.range.start.column = startPosition.column;
		diag.range.end.line = endPosition.line;
		diag.range.end.column = endPosition.column;
		diag.message = message.primary.message;
		diag.source = "solc";
		diag.severity = severity;

		for (SourceReference const& secondary: message.secondary)
		{
			auto related = DiagnosticRelatedInformation{};

			related.message = secondary.message;
			related.location.path = secondary.sourceName; // is the sourceName always a fully qualified path?
			related.location.range.start.line = secondary.position.line;
			related.location.range.start.column = secondary.startColumn;
			related.location.range.end.line = secondary.position.line; // what about multiline?
			related.location.range.end.column = secondary.endColumn;

			diag.relatedInformation.emplace_back(move(related));
		}

		if (message.errorId.has_value())
			diag.code = message.errorId.value().error;

		collectedDiagnostics.emplace_back(move(diag));
	}
	optional<int> version = nullopt; // TODO: do we need this? (AFAIK that's been the file version this diagnostics belongs to)
	pushDiagnostics(_file.path(), version, collectedDiagnostics);
}

frontend::ASTNode const* LanguageServer::findASTNode(Position const& _position, std::string const& _fileName)
{
	if (!m_compilerStack)
		return nullptr;

	frontend::ASTNode const& sourceUnit = m_compilerStack->ast(_fileName);
	auto const sourcePos = sourceUnit.location().source->translateLineColumnToPosition(_position.line + 1, _position.column + 1);

	ASTNodeLocator m{sourcePos};
	sourceUnit.accept(m);
	auto const closestMatch = m.closestMatch();

	trace(
		"findASTNode not found for "s +
		to_string(sourcePos) + ":" +
		to_string(_position.line) + ":" +
		to_string(_position.column)
	);

	return closestMatch;
}

std::vector<Location> LanguageServer::gotoDefinition(DocumentPosition _location)
{
	auto const file = m_vfs.find(_location.path);
	if (!file)
		return {};

	// source should be compiled already
	solAssert(m_compilerStack.get() != nullptr, "");

	auto const sourceName = file->path();
	auto const sourceNode = findASTNode(_location.position, sourceName);
	if (!sourceNode)
		return {}; // Could not infer AST node from given source location.

	if (auto const importDirective = dynamic_cast<ImportDirective const*>(sourceNode))
	{
		// When cursor is on an import directive, then we want to jump to the actual file that
		// is being imported.
		auto const fpm = m_fileReader->fullPathMapping().find(importDirective->path());
		if (fpm == m_fileReader->fullPathMapping().end())
		{
			trace("gotoDefinition: (importDirective) full path mapping not found\n");
			return {}; // definition not found
		}

		Location output{};
		output.path = fpm->second;
		return {output};
	}
	else if (auto const n = dynamic_cast<frontend::MemberAccess const*>(sourceNode))
	{
		// For scope members, jump to the naming symbol of the referencing declaration of this member.
		auto const declaration = n->annotation().referencedDeclaration;

		auto const location = declarationPosition(declaration);
		if (!location.has_value())
		{
			trace("gotoDefinition: declaration not found.");
			return {}; // definition not found
		}

		return {location.value()};
	}
	else if (auto const sourceIdentifier = dynamic_cast<Identifier const*>(sourceNode))
	{
		// For identifiers, jump to the naming symbol of the definition of this identifier.
		vector<Location> output;

		if (auto location = declarationPosition(sourceIdentifier->annotation().referencedDeclaration); location.has_value())
			output.emplace_back(move(location.value()));

		for (auto const declaration: sourceIdentifier->annotation().candidateDeclarations)
			if (auto location = declarationPosition(declaration); location.has_value())
				output.emplace_back(move(location.value()));

		return output;
	}
	else
	{
		trace("gotoDefinition: Symbol is not an identifier. "s + typeid(*sourceIdentifier).name());
		return {};
	}
}

optional<Location> LanguageServer::declarationPosition(frontend::Declaration const* _declaration)
{
	if (!_declaration)
		return nullopt;

	auto const location = _declaration->nameLocation();
	auto const [startLine, startColumn] = location.source->translatePositionToLineColumn(location.start);
	auto const [endLine, endColumn] = location.source->translatePositionToLineColumn(location.end);

	auto const sourceName = _declaration->location().source->name();

	auto output = Location{};
	if (auto fullPath = m_fileReader->fullPathMapping().find(sourceName); fullPath != m_fileReader->fullPathMapping().end())
		output.path = fullPath->second;
	else
		output.path = sourceName;

	output.range = {
		{startLine, startColumn},
		{endLine, endColumn}
	};

	return output;
}

void LanguageServer::findAllReferences(
	frontend::Declaration const* _declaration,
	string const& _sourceIdentifierName,
	frontend::SourceUnit const& _sourceUnit,
	string const& _sourceUnitPath,
	std::vector<Location>& _output
)
{
	for (auto const& highlight: ReferenceCollector::collect(_declaration, _sourceUnit, _sourceIdentifierName))
	{
		auto location = Location{};
		location.range = highlight.range;
		location.path = _sourceUnitPath;
		_output.emplace_back(location);
	}
}

vector<Location> LanguageServer::references(DocumentPosition _documentPosition)
{
	trace(
		"find all references: " +
		_documentPosition.path + ":" +
		to_string(_documentPosition.position.line) + ":" +
		to_string(_documentPosition.position.column)
	);

	auto const file = m_vfs.find(_documentPosition.path);
	if (!file)
	{
		trace("File does not exist. " + _documentPosition.path);
		return {};
	}

	if (!m_compilerStack)
		compile(*file);

	solAssert(m_compilerStack.get() != nullptr, "");

	auto const sourceName = file->path();

	auto const sourceNode = findASTNode(_documentPosition.position, sourceName);
	if (!sourceNode)
	{
		trace("AST node not found");
		return {};
	}

	auto output = vector<Location>{};
	if (auto const sourceIdentifier = dynamic_cast<Identifier const*>(sourceNode))
	{
		auto const sourceName = _documentPosition.path;
		frontend::SourceUnit const& sourceUnit = m_compilerStack->ast(sourceName);

		if (auto decl = sourceIdentifier->annotation().referencedDeclaration)
			findAllReferences(decl, decl->name(), sourceUnit, _documentPosition.path, output);

		for (auto const decl: sourceIdentifier->annotation().candidateDeclarations)
			findAllReferences(decl, decl->name(), sourceUnit, _documentPosition.path, output);
	}
	else if (auto const decl = dynamic_cast<VariableDeclaration const*>(sourceNode))
	{
		auto const sourceName = _documentPosition.path;
		frontend::SourceUnit const& sourceUnit = m_compilerStack->ast(sourceName);
		findAllReferences(decl, decl->name(), sourceUnit, _documentPosition.path, output);
	}
	else if (auto const* functionDefinition = dynamic_cast<FunctionDefinition const*>(sourceNode))
	{
		frontend::SourceUnit const& sourceUnit = m_compilerStack->ast(sourceName);
		findAllReferences(functionDefinition, functionDefinition->name(), sourceUnit, _documentPosition.path, output);
	}
	else if (auto const* enumDef = dynamic_cast<EnumDefinition const*>(sourceNode))
	{
		frontend::SourceUnit const& sourceUnit = m_compilerStack->ast(sourceName);
		findAllReferences(enumDef, enumDef->name(), sourceUnit, _documentPosition.path, output);
	}
	else if (auto const memberAccess = dynamic_cast<MemberAccess const*>(sourceNode))
	{
		if (Declaration const* decl = memberAccess->annotation().referencedDeclaration)
		{
			auto const sourceName = _documentPosition.path;
			frontend::SourceUnit const& sourceUnit = m_compilerStack->ast(sourceName);
			findAllReferences(decl, memberAccess->memberName(), sourceUnit, _documentPosition.path, output);
		}
	}
	else
		trace("references: not an identifier: "s + typeid(*sourceNode).name());

	return output;
}

vector<DocumentHighlight> LanguageServer::semanticHighlight(DocumentPosition _documentPosition)
{
	auto const file = m_vfs.find(_documentPosition.path);
	if (!file)
	{
		// reply(_params.requestId, output);
		// m_client.error(_documentPosition.requestId, ErrorCode::RequestCancelled, "not implemented yet.");
		return {};
	}

	compile(*file);
	solAssert(m_compilerStack.get() != nullptr, "");

	auto const sourceName = file->path();

	auto const sourceNode = findASTNode(_documentPosition.position, sourceName);
	if (!sourceNode)
	{
		trace("semanticHighlight: AST node not found");
		// m_client.error(_documentPosition.requestId, ErrorCode::InvalidParams, "Symbol not found.");
		return {};
	}

	trace(
		"semanticHighlight: Source Node("s + typeid(*sourceNode).name() + "): " +
		sourceNode->location().text()
	);

	auto output = vector<DocumentHighlight>{};

	// TODO: ImportDirective: hovering a symbol of an import directive should highlight all uses of that symbol.
	if (auto const* sourceIdentifier = dynamic_cast<Identifier const*>(sourceNode))
	{
		auto const sourceName = _documentPosition.path;
		frontend::SourceUnit const& sourceUnit = m_compilerStack->ast(sourceName);

		if (sourceIdentifier->annotation().referencedDeclaration)
			output += ReferenceCollector::collect(sourceIdentifier->annotation().referencedDeclaration, sourceUnit, sourceIdentifier->name());

		for (Declaration const* declaration: sourceIdentifier->annotation().candidateDeclarations)
			output += ReferenceCollector::collect(declaration, sourceUnit, sourceIdentifier->name());

		for (Declaration const* declaration: sourceIdentifier->annotation().overloadedDeclarations)
			output += ReferenceCollector::collect(declaration, sourceUnit, sourceIdentifier->name());
	}
	else if (auto const* varDecl = dynamic_cast<VariableDeclaration const*>(sourceNode))
	{
		auto const sourceName = _documentPosition.path;
		frontend::SourceUnit const& sourceUnit = m_compilerStack->ast(sourceName);
		output = ReferenceCollector::collect(varDecl, sourceUnit, varDecl->name());
	}
	else if (auto const* memberAccess = dynamic_cast<MemberAccess const*>(sourceNode))
	{
		TypePointer const type = memberAccess->expression().annotation().type;
		if (auto const ttype = dynamic_cast<TypeType const*>(type))
		{
			auto const memberName = memberAccess->memberName();

			auto const sourceName = _documentPosition.path;
			frontend::SourceUnit const& sourceUnit = m_compilerStack->ast(sourceName);

			if (auto const* enumType = dynamic_cast<EnumType const*>(ttype->actualType()))
			{
				auto const& enumMembers = enumType->enumDefinition().members();
				if (enumMembers.empty())
					trace("enumType members are empty");

				// find the definition
				for (auto const& enumMember: enumMembers)
					if (enumMember->name() == memberName)
						output += ReferenceCollector::collect(enumMember.get(), sourceUnit, enumMember->name());

				// find uses of the enum value
			}
			else
				trace("semanticHighlight: not an EnumType");
		}
		else
			trace("semanticHighlight: member type is NULL");

		// TODO: If the cursor os positioned on top of a type name, then all other symbols matching
		// this type should be highlighted (clangd does so, too).
		//
		// if (auto const tt = dynamic_cast<TypeType const*>(type))
		// {
		// 	auto const sourceName = _documentPosition.path;
		// 	frontend::SourceUnit const& sourceUnit = m_compilerStack->ast(sourceName);
		// 	output = findAllReferences(declaration, sourceUnit);
		// }
	}
	else if (auto const* identifierPath = dynamic_cast<IdentifierPath const*>(sourceNode))
	{
		solAssert(!identifierPath->path().empty(), "");
		frontend::SourceUnit const& sourceUnit = m_compilerStack->ast(sourceName);
		output += ReferenceCollector::collect(identifierPath->annotation().referencedDeclaration, sourceUnit, identifierPath->path().back());
	}
	else if (auto const* functionDefinition = dynamic_cast<FunctionDefinition const*>(sourceNode))
	{
		frontend::SourceUnit const& sourceUnit = m_compilerStack->ast(sourceName);
		output += ReferenceCollector::collect(functionDefinition, sourceUnit, functionDefinition->name());
	}
	else if (auto const* enumDef = dynamic_cast<EnumDefinition const*>(sourceNode))
	{
		frontend::SourceUnit const& sourceUnit = m_compilerStack->ast(sourceName);
		output += ReferenceCollector::collect(enumDef, sourceUnit, enumDef->name());
	}
	else
		trace("semanticHighlight: not an identifier. "s + typeid(*sourceNode).name());

	return output;
}

// {{{ LSP internals
bool LanguageServer::run()
{
	while (!m_exitRequested && !m_client.closed())
	{
		// TODO: receive() must return a variant<> to also return on <Transport::TimeoutEvent>,
		// so that we can perform some idle tasks in the meantime, such as
		// - lazy validation runs
		// - check for results of asynchronous runs (in case we want to support threaded background jobs)
		// Also, EOF should be noted properly as a <Transport::ClosedEvent>.
		optional<Json::Value> const jsonMessage = m_client.receive();
		if (jsonMessage.has_value())
		{
			try
			{
				handleMessage(*jsonMessage);
			}
			catch (std::exception const& e)
			{
				log("Unhandled exception caught when handling message. "s + e.what());
			}
		}
		else
			log("Could not read RPC request.");
	}

	if (m_shutdownRequested)
		return true;
	else
		return false;
}

void LanguageServer::handle_initialize(MessageId _id, Json::Value const& _args)
{
	string rootPath;
	if (Json::Value uri = _args["rootUri"])
		rootPath = extractPathFromFileURI(uri.asString()).value();
	else if (Json::Value rootPath = _args["rootPath"]; rootPath)
		rootPath = rootPath.asString();

	if (Json::Value value = _args["trace"]; value)
	{
		string const name = value.asString();
		if (name == "messages")
			m_trace = Trace::Messages;
		else if (name == "verbose")
			m_trace = Trace::Verbose;
		else if (name == "off")
			m_trace = Trace::Off;
	}

#if 0 // Currently not used.
	// At least VScode supports more than one workspace.
	// This is the list of initial configured workspace folders
	struct WorkspaceFolder { std::string name; std::string path; };
	std::vector<WorkspaceFolder> workspaceFolders;
	if (Json::Value folders = _args["workspaceFolders"]; folders)
	{
		for (Json::Value folder: folders)
		{
			WorkspaceFolder wsFolder{};
			wsFolder.name = folder["name"].asString();
			wsFolder.path = extractPathFromFileURI(folder["uri"].asString()).value();
			workspaceFolders.emplace_back(move(wsFolder));
		}
	}
#endif

	// TODO: ClientCapabilities
	// ... Do we actually care? Not in the initial PR.

	auto const fspath = boost::filesystem::path(rootPath);

	m_basePath = fspath;
	m_allowedDirectories.push_back(fspath);

	if (_args["initializationOptions"].isObject())
		changeConfiguration(_args["initializationOptions"]);

	// {{{ encoding
	Json::Value replyArgs;

	replyArgs["serverInfo"]["name"] = "solc";
	replyArgs["serverInfo"]["version"] = string(solidity::frontend::VersionNumber);
	replyArgs["hoverProvider"] = true;
	replyArgs["capabilities"]["hoverProvider"] = true;
	replyArgs["capabilities"]["textDocumentSync"]["openClose"] = true;
	replyArgs["capabilities"]["textDocumentSync"]["change"] = 2; // 0=none, 1=full, 2=incremental
	replyArgs["capabilities"]["definitionProvider"] = true;
	replyArgs["capabilities"]["documentHighlightProvider"] = true;
	replyArgs["capabilities"]["referencesProvider"] = true;

	m_client.reply(_id, replyArgs);
	// }}}
}

void LanguageServer::handle_workspace_didChangeConfiguration(MessageId, Json::Value const& _args)
{
	if (_args["settings"].isObject())
		changeConfiguration(_args["settings"]);
}

void LanguageServer::handle_exit(MessageId _id, Json::Value const& /*_args*/)
{
	m_exitRequested = true;
	auto const exitCode = m_shutdownRequested ? 0 : 1;

	Json::Value replyArgs = Json::intValue;
	replyArgs = exitCode;

	m_client.reply(_id, replyArgs);
}

void LanguageServer::handle_textDocument_didOpen(MessageId /*_id*/, Json::Value const& _args)
{
	// decoding
	if (!_args["textDocument"])
		return;

	auto const path = extractPathFromFileURI(_args["textDocument"]["uri"].asString()).value();
	auto const languageId = _args["textDocument"]["languageId"].asString();
	auto const version = _args["textDocument"]["version"].asInt();
	auto const text = _args["textDocument"]["text"].asString();

	log("LanguageServer: Opening document: " + path);

	vfs::File const& file = m_vfs.insert(
		path,
		languageId,
		version,
		text
	);

	validate(file);

	// no encoding
}

void LanguageServer::handle_textDocument_didChange(MessageId /*_id*/, Json::Value const& _args)
{
	auto const version = _args["textDocument"]["version"].asInt();
	auto const path = extractPathFromFileURI(_args["textDocument"]["uri"].asString()).value();

	// TODO: in the longer run, I'd like to try moving the VFS handling into Server class, so
	// the specific Solidity LSP implementation doesn't need to care about that.

	auto const contentChanges = _args["contentChanges"];
	for (Json::Value jsonContentChange: contentChanges)
	{
		if (!jsonContentChange.isObject())
			// Protocol error, will only happen on broken clients, so silently ignore it.
			continue;

		auto const text = jsonContentChange["text"].asString();

		if (jsonContentChange["range"].isObject())
		{
			Json::Value jsonRange = jsonContentChange["range"];
			Range range{};
			range.start.line = jsonRange["start"]["line"].asInt();
			range.start.column = jsonRange["start"]["character"].asInt();
			range.end.line = jsonRange["end"]["line"].asInt();
			range.end.column = jsonRange["end"]["character"].asInt();

			documentContentUpdated(path, version, range, text);
		}
		else
		{
			// full content update
			documentContentUpdated(path, version, text);
		}
	}

	if (!contentChanges.empty())
	{
		auto file = m_vfs.find(path);
		if (!file)
			log("LanguageServer: File to be modified not opened \"" + path + "\"");
		else
			validate(*file);
	}
}

void LanguageServer::handle_textDocument_definition(MessageId _id, Json::Value const& _args)
{
	DocumentPosition dpos{};
	loadTextDocumentPosition(dpos, _args);

	Json::Value reply = Json::arrayValue;
	for (Location const& target: gotoDefinition(dpos))
	{
		Json::Value json = Json::objectValue;
		json["uri"] = toFileURI(target.path);
		json["range"]["start"]["line"] = target.range.start.line;
		json["range"]["start"]["character"] = target.range.start.column;
		json["range"]["end"]["line"] = target.range.end.line;
		json["range"]["end"]["character"] = target.range.end.column;
		reply.append(json);
	}
	m_client.reply(_id, reply);
}

void LanguageServer::handle_textDocument_highlight(MessageId _id, Json::Value const& _args)
{
	DocumentPosition dpos{};
	loadTextDocumentPosition(dpos, _args);

	Json::Value replyArgs = Json::arrayValue;
	for (DocumentHighlight const& highlight: semanticHighlight(dpos))
	{
		Json::Value item = Json::objectValue;

		item["range"]["start"]["line"] = highlight.range.start.line;
		item["range"]["start"]["character"] = highlight.range.start.column;
		item["range"]["end"]["line"] = highlight.range.end.line;
		item["range"]["end"]["character"] = highlight.range.end.column;

		if (highlight.kind != DocumentHighlightKind::Unspecified)
			item["kind"] = static_cast<int>(highlight.kind);

		replyArgs.append(item);
	}
	m_client.reply(_id, replyArgs);
}

void LanguageServer::handle_textDocument_references(MessageId _id, Json::Value const& _args)
{
	DocumentPosition dpos{};
	loadTextDocumentPosition(dpos, _args);

	Json::Value replyArgs = Json::arrayValue;
	for (Location const& location: references(dpos))
	{
		Json::Value item = Json::objectValue;

		item["range"]["start"]["line"] = location.range.start.line;
		item["range"]["start"]["character"] = location.range.start.column;
		item["range"]["end"]["line"] = location.range.end.line;
		item["range"]["end"]["character"] = location.range.end.column;
		item["uri"] = toFileURI(location.path);

		replyArgs.append(item);
	}
	m_client.reply(_id, replyArgs);
}

void LanguageServer::pushDiagnostics(string const& _path, optional<int> _version, vector<Diagnostic> const& _diagnostics)
{
	Json::Value params;

	params["uri"] = toFileURI(_path);

	if (_version)
		params["version"] = _version.value();

	params["diagnostics"] = Json::arrayValue;
	for (Diagnostic const& diag: _diagnostics)
	{
		Json::Value jsonDiag;

		jsonDiag["range"] = toJson(diag.range);

		if (diag.severity.has_value())
			jsonDiag["severity"] = static_cast<int>(diag.severity.value());

		if (diag.code.has_value())
			jsonDiag["code"] = Json::UInt64{diag.code.value()};

		if (diag.source.has_value())
			jsonDiag["source"] = diag.source.value();

		jsonDiag["message"] = diag.message;

		if (!diag.diagnosticTag.empty())
			for (DiagnosticTag tag: diag.diagnosticTag)
				jsonDiag["diagnosticTag"].append(static_cast<int>(tag));

		if (!diag.relatedInformation.empty())
		{
			for (DiagnosticRelatedInformation const& related: diag.relatedInformation)
			{
				Json::Value json;
				json["message"] = related.message;
				json["location"]["uri"] = toFileURI(related.location.path);
				json["location"]["range"] = toJson(related.location.range);
				jsonDiag["relatedInformation"].append(json);
			}
		}

		params["diagnostics"].append(jsonDiag);
	}

	m_client.notify("textDocument/publishDiagnostics", params);
}

void LanguageServer::log(string const& _message)
{
	if (m_trace < Trace::Messages)
		return;

	Json::Value json = Json::objectValue;
	json["type"] = static_cast<int>(Trace::Messages);
	json["message"] = _message;

	m_client.notify("window/logMessage", json);

	if (m_logger)
		m_logger(_message);
}

void LanguageServer::trace(string const& _message)
{
	if (m_trace < Trace::Verbose)
		return;

	Json::Value json = Json::objectValue;
	json["type"] = static_cast<int>(Trace::Verbose);
	json["message"] = _message;

	m_client.notify("window/logMessage", json);

	if (m_logger)
		m_logger(_message);
}

void LanguageServer::handleMessage(Json::Value const& _jsonMessage)
{
	string const methodName = _jsonMessage["method"].asString();

	MessageId const id = _jsonMessage["id"].isInt()
		? MessageId{_jsonMessage["id"].asInt()}
		: _jsonMessage["id"].isString()
			? MessageId{_jsonMessage["id"].asString()}
			: MessageId{};

	if (auto const handler = m_handlers.find(methodName); handler != m_handlers.end() && handler->second)
	{
		Json::Value const& jsonArgs = _jsonMessage["params"];
		handler->second(id, jsonArgs);
	}
	else
		m_client.error(id, ErrorCode::MethodNotFound, "Unknown method " + methodName);
}
// }}}

} // namespace solidity

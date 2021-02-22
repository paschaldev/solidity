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
#pragma once

#include <optional>
#include <ostream>
#include <string>
#include <vector>

namespace solidity::lsp
{

/**
 * Position in a text document expressed as zero-based line and zero-based
 * character offset. A position is between two characters like an ‘insert’ cursor
 * in a editor. Special values like for example -1 to denote the end of a line
 * are not supported.
 */
struct Position
{
	int line;    // zero-based index to the line
	int column;  // zero-based index to the column

	constexpr bool operator==(Position _other) const noexcept { return line == _other.line && column == _other.column; }
	constexpr bool operator!=(Position _other) const noexcept { return !(*this == _other); }
};

inline std::ostream& operator<<(std::ostream& _output, Position _pos)
{
	return _output << '(' << (_pos.line + 1) << (_pos.column + 1) << ')';
}

/**
 * A range in a text document expressed as (zero-based) start and end positions.
 *
 * A range is comparable to a selection in an editor. Therefore the end position is exclusive.
 * If you want to specify a range that contains a line including the line ending character(s)
 * then use an end position denoting the start of the next line. For example:
 *
 * {
 *   start: { line: 5, character: 23 },
 *   end : { line 6, character : 0 }
 * }
 */
struct Range
{
	struct LineNumIterator {
		int current;
		int lastLine;

		/// Determines whether or not this is an inner line or a boundary line (first/last).
		bool inner = false;

		constexpr int operator*() const noexcept { return current; }

		constexpr LineNumIterator& operator++() noexcept
		{
			++current;
			if (current + 1 < lastLine)
				inner = true;
			return *this;
		}

		constexpr LineNumIterator& operator++(int) noexcept
		{
			return ++*this;
		}

		constexpr bool operator==(LineNumIterator const& _rhs) const noexcept
		{
			return current == _rhs.current;
		}

		constexpr bool operator!=(LineNumIterator const& _rhs) const noexcept
		{
			return !(*this == _rhs);
		}
	};

	/// Returns an iterator for iterating through the line numbers of this range.
	[[nodiscard]] constexpr LineNumIterator lineNumbers() const noexcept
	{
		return LineNumIterator{start.line, end.line + 1};
	}

	Position start;
	Position end;
};

inline std::ostream& operator<<(std::ostream& _output, Range _range)
{
	return _output << _range.start << ".." << _range.end;
}

enum class Trace { Off, Messages, Verbose };

struct DocumentPosition {
	std::string path;
	Position position;
};

struct ServerId {
	std::string serverName;
	std::string serverVersion;
};

enum class DocumentHighlightKind {
	Unspecified,
	Text,           //!< a textual occurrence
	Read,           //!< read access to a variable
	Write,          //!< write access to a variable
};

struct Location {
	std::string path;
	Range range;
};

struct DocumentHighlight {
	Range range;
	DocumentHighlightKind kind = DocumentHighlightKind::Unspecified;
};

enum class DiagnosticSeverity {
	Error = 1,
	Warning = 2,
	Information = 3,
	Hint = 4,
};

enum class DiagnosticTag {
	Unnecessary = 1, // Unused or unnecessary code.
	Deprecated = 2   // Deprecated or obsolete code.
};

/// Represents a related message and source code location for a diagnostic. This should be
/// used to point to code locations that cause or related to a diagnostics, e.g when duplicating
/// a symbol in a scope.
struct DiagnosticRelatedInformation {
	Location location;   // The location of this related diagnostic information.
	std::string message; // The message of this related diagnostic information.
};

/// Represents a diagnostic, such as a compiler error or warning. Diagnostic objects are only valid in the scope of a resource.
struct Diagnostic {
	Range range;                                   // The range at which the message applies.
	std::optional<DiagnosticSeverity> severity;
	std::optional<unsigned long long> code;        // The diagnostic's code, which might appear in the user interface.
	std::optional<std::string> source;             // A human-readable string describing the source of this diagnostic, e.g. 'typescript' or 'super lint'.
	std::string message;                           // The diagnostic's message.
	std::vector<DiagnosticTag> diagnosticTag;      // Additional metadata about the diagnostic.
	std::vector<DiagnosticRelatedInformation> relatedInformation; // An array of related diagnostic information, e.g. when symbol-names within a scope collide all definitions can be marked via this property.
};

}

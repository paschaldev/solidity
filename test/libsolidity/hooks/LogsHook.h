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

#pragma once

#include <test/libsolidity/TestHook.h>
#include <test/libsolidity/util/SoltestTypes.h>

#include <map>
#include <set>
#include <vector>

namespace solidity::frontend::test
{
class SemanticTest;
class SolidityExecutionFramework;

class LogsHook: public TestHook
{
public:
	LogsHook(SemanticTest* _test);
	~LogsHook() override = default;

	std::optional<bytes> numLogs(FunctionCall const& _call);
	std::optional<bytes> numLogTopics(FunctionCall const& _call);
	std::optional<bytes> logTopic(FunctionCall const& _call);
	std::optional<bytes> logAddress(FunctionCall const& _call);
	std::optional<bytes> logData(FunctionCall const& _call);
	std::optional<bytes> expectEvent(FunctionCall const& _call);

	void beginTestCase() override;
	void afterFunctionCall(TestFunctionCall const&) override;

	void beforeFunctionCall(TestFunctionCall const&) override {}
	void endTestCase() override {}

	bool verifyFunctionCall(TestFunctionCall const&) override { return true; }

	std::string formatFunctionCall(
		TestFunctionCall const&, ErrorReporter&, std::string const&, const bool, const bool) const override
	{
		return "";
	}

private:
	SemanticTest* m_test = nullptr;
	SolidityExecutionFramework* m_executionFramework = nullptr;

	std::map<FunctionCall const*, std::vector<LogRecord>> m_producedLogs{};
	std::map<FunctionCall const*, std::set<size_t>> m_touchedLogs{};
};

} // namespace  solidity::frontend::test

#include "pch.hpp"

#include "gen/mylang.hpp"

#include <iostream>
#include <cassert>
#include <bit>

namespace vm
{

struct State
{
	struct StateBlock
	{
		std::unordered_map<std::string, int64_t> variables;
	};

	struct StateFunction
	{
		std::string name;
		std::vector<StateBlock> blocks;
	};

	std::vector<StateFunction> functions;

	int64_t *declare_or_get_variable(const std::string &name)
	{
		assert(functions.size() > 0);
		assert(functions[0].blocks.size() > 0);

		return &functions.back().blocks.back().variables[name];
	}

	int64_t *find_variable(const std::string &name)
	{
		assert(functions.size() > 0);
		assert(functions[0].blocks.size() > 0);

		// ignore variables from caller functions
		auto &blocks = functions.back().blocks;

		for (auto it = blocks.rbegin(); it != blocks.rend(); ++it)
		{
			if (auto v = it->variables.find(name); v != it->variables.end())
			{
				return &v->second;
			}
		}

		return nullptr;
	}
};

using func_t = int64_t(*)(const std::vector<int64_t> &args, State &state);

int64_t evaluate_call(int64_t func, const std::string &function_name, const std::vector<int64_t> &args, State &state)
{
	int64_t result = 0;
	func_t func_ptr = std::bit_cast<func_t>(func);

	auto func_layer = state.functions.size() + 1;

	{
		auto &func = state.functions.emplace_back();
		func.name = function_name;
		func.blocks.emplace_back();
	}

	{
		result = func_ptr(args, state);
	}

	{
		assert(func_layer == state.functions.size());
		assert(state.functions.back().blocks.size() == 1);
		state.functions.pop_back();
	}

	return result;
}

int64_t evaluate_expr(const mylang::$$Parsed &par, State &state)
{
	if (par.identifier == "expr")
	{
		const auto &expr = par;

		if (expr.group[0].identifier == "expr_call")
		{
			const auto &expr_call = expr.group[0];
			return evaluate_expr(expr_call, state);
		}
		else
		if (expr.group[0].identifier == "expr_add")
		{
			const auto &expr_add = expr.group[0];
			return evaluate_expr(expr_add, state);
		}
		else
		{
			throw 1;
		}
	}
	else
	if (par.identifier == "expr_add")
	{
		const auto &expr_add = par;
		const auto &expr_mul = expr_add.group[0];

		auto result = evaluate_expr(expr_mul, state);

		for (size_t i = 1; i < expr_add.group.size(); ++i)
		{
			const auto &expr_add_$g0 = expr_add.group[i];
			const auto &expr_add_$g0_$g0 = expr_add_$g0.group[0];

			const auto &sign = expr_add_$g0_$g0.group[0];
			const auto &expr_mul = expr_add_$g0.group[1];

			auto val = evaluate_expr(expr_mul, state);

			if (sign.literal == "+")
			{
				result += val;
			}
			else
			if (sign.literal == "-")
			{
				result -= val;
			}
		}

		return result;
	}
	else
	if (par.identifier == "expr_mul")
	{
		const auto &expr_mul = par;
		const auto &expr_primitive = expr_mul.group[0];

		auto result = evaluate_expr(expr_primitive, state);

		for (size_t i = 1; i < expr_mul.group.size(); ++i)
		{
			const auto &expr_mul_$g0 = expr_mul.group[i];
			const auto &expr_mul_$g0_$g0 = expr_mul_$g0.group[0];

			const auto &sign = expr_mul_$g0_$g0.group[0];
			const auto &expr_primitive = expr_mul_$g0.group[1];

			auto val = evaluate_expr(expr_primitive, state);

			if (sign.literal == "*")
			{
				result *= val;
			}
			else
			if (sign.literal == "/")
			{
				result /= val;
			}
		}

		return result;
	}
	else
	if (par.identifier == "expr_primitive")
	{
		const auto &expr_primitive = par;

		if (expr_primitive.group[0].identifier == "number")
		{
			const auto &number = expr_primitive.group[0];
			return evaluate_expr(number, state);
		}
		else
		if (expr_primitive.group[0].identifier == "token_path")
		{
			const auto &token_path = expr_primitive.group[0];
			return evaluate_expr(token_path, state);
		}
		else
		if (expr_primitive.group[0].identifier == "expr_group")
		{
			const auto &expr_group = expr_primitive.group[0];
			return evaluate_expr(expr_group, state);
		}
		else
		{
			throw 1;
		}
	}
	else
	if (par.identifier == "number")
	{
		const auto &number = par;
		return (double)atoi(number.flatten().c_str());
	}
	else
	if (par.identifier == "token_path")
	{
		const auto &token_path = par;
		return *state.find_variable(token_path.flatten());
	}
	else
	if (par.identifier == "expr_group")
	{
		const auto &expr_group = par;
		const auto &expr = expr_group.group[1];
		return evaluate_expr(expr, state);
	}
	else
	if (par.identifier == "expr_call")
	{
		const auto &expr_call = par;
		const auto &expr_call_$g0 = expr_call.group[0];

		int64_t func = 0;
		std::string function_name;

		if (expr_call_$g0.group[0].identifier == "token_path")
		{
			const auto &token_path = expr_call_$g0.group[0];
			function_name = token_path.flatten();
			func = *state.find_variable(function_name);
		}
		else
		if (expr_call_$g0.identifier == "expr_group")
		{
			const auto &expr_group = expr_call_$g0.group[0];
			func = evaluate_expr(expr_group, state);
		}
		else
		{
			throw 1;
		}

		std::vector<int64_t> args;

		if (const auto *expr_call_$g1 = expr_call.find("expr_call_$g1"))
		{
			const auto &expr = expr_call_$g1->group[0];
			args.push_back(evaluate_expr(expr, state));

			for (size_t i = 1; i < expr_call_$g1->group.size(); ++i)
			{
				const auto &expr_call_$g1_$g0 = expr_call_$g1->group[i];

				const auto &expr = expr_call_$g1_$g0.group[1];
				args.push_back(evaluate_expr(expr, state));
			}
		}

		return evaluate_call(func, function_name, args, state);
	}
	else
	{
		throw 1;
	}
}

void evaluate_statement(const mylang::$$Parsed &statement, State &state)
{
	if (statement.group[0].identifier == "statement_assign")
	{
		const auto &statement_assign = statement.group[0];
		const auto &token_path = statement_assign.group[0];
		const auto &expr = statement_assign.group[2];

		auto result = evaluate_expr(expr, state);
		*state.declare_or_get_variable(token_path.flatten()) = result;
	}
	else
	if (statement.group[0].identifier == "statement_expr")
	{
		const auto &statement_expr = statement.group[0];
		const auto &expr = statement_expr.group[0];
		auto result = evaluate_expr(expr, state);
		(void)result;
	}
	else
	{
		throw 1;
	}
}

void evaluate_root(const mylang::$$Parsed &root)
{
	State state;

	{
		auto &func = state.functions.emplace_back();
		func.name = "<global>";
		func.blocks.emplace_back();
	}

	{
		func_t func = [](const std::vector<int64_t> &args, State &state) -> int64_t {
			for (size_t i = 0; i < args.size(); ++i)
			{
				if (i != 0)
					std::cout << " ";

				std::cout << args[i];
			}

			std::cout << "\n";

			return 0;
		};

		*state.declare_or_get_variable("std.print") = std::bit_cast<int64_t>(func);
	}

	for (size_t i = 0; i < root.group.size(); ++i)
	{
		const auto &root_$g0 = root.group[i];
		const auto &statement = root_$g0.group[0];
		evaluate_statement(statement, state);
	}

	assert(state.functions.size() == 1);
	assert(state.functions[0].blocks.size() == 1);
}


} // namespace vm

void mylang_main()
{
	std::string_view text = R"AAA(

myprint = std.print;
myprint(myprint);

a = 3;
b = 4;
c = 5;
d = 6;

std.print(a + b * c + d);
std.print(a + b * (c + d));
std.print((a + b) * c + d);
std.print((a + b) * (c + d));


)AAA";

	const char *s = text.data();
	const char *e = s + text.size();

	auto result = mylang::$parse_root(s, e);

	vm::evaluate_root(result.value());

	// std::cout << mylang::helpers::generate_graphviz(result.value());

	int a = 0;
}

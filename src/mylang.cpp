#include "pch.hpp"

#include "gen/mylang.hpp"

#include <iostream>
#include <cassert>
#include <bit>
#include <memory>
#include <unordered_set>

namespace vm
{

struct Lambda;
struct State;

using func_t = int64_t(*)(const std::vector<int64_t> &args, State &state);
using lambda_t = int64_t(*)(const Lambda *lambda, const std::vector<int64_t> &args, State &state);

struct Lambda
{
	lambda_t lambda_ptr = nullptr;
	const mylang::$$Parsed *expr_block = nullptr;
	std::vector<std::string> args;
};

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

	std::vector<StateFunction> function_scopes;
	std::unordered_set<std::unique_ptr<Lambda>> lambdas;
	int64_t last_return = 0;

	Lambda *create_lambda(lambda_t lambda)
	{
		auto it = lambdas.insert(std::make_unique<Lambda>()).first;
		it->get()->lambda_ptr = lambda;
		return it->get();
	}

	Lambda *create_lambda(std::string name, lambda_t lambda)
	{
		auto l = create_lambda(lambda);
		*declare_or_get_variable(name) = std::bit_cast<int64_t>(l);
		return l;
	}

	int64_t *declare_or_get_variable(const std::string &name)
	{
		assert(function_scopes.size() > 0);
		assert(function_scopes[0].blocks.size() > 0);

		return &function_scopes.back().blocks.back().variables[name];
	}

	int64_t *find_variable(const std::string &name)
	{
		assert(function_scopes.size() > 0);
		assert(function_scopes[0].blocks.size() > 0);

		// ignore variables from caller functions
		auto &blocks = function_scopes.back().blocks;

		for (auto it = blocks.rbegin(); it != blocks.rend(); ++it)
		{
			if (auto v = it->variables.find(name); v != it->variables.end())
			{
				return &v->second;
			}
		}

		return nullptr;
	}

	Lambda *find_lambda(int64_t func)
	{
		auto l = ((std::unordered_set<int64_t> *)&lambdas);

		if (auto v = l->find(func); v != l->end())
			return std::bit_cast<Lambda *>(*v);

		return nullptr;
	}
};

void evaluate_statement(const mylang::$$Parsed &statement, State &state);

int64_t evaluate_call(int64_t func, const std::string &function_name, const std::vector<int64_t> &args, State &state)
{
	int64_t result = 0;

	auto func_layer = state.function_scopes.size() + 1;

	{
		auto &func = state.function_scopes.emplace_back();
		func.name = function_name;
		func.blocks.emplace_back();
	}

	{
		if (const auto *lambda = state.find_lambda(func))
		{
			// user defined functions
			lambda_t lambda_ptr = lambda->lambda_ptr;
			result = lambda_ptr(lambda, args, state);
		}
		else
		{
			// precompiled functions
			func_t func_ptr = std::bit_cast<func_t>(func);
			result = func_ptr(args, state);
		}
	}

	{
		assert(func_layer == state.function_scopes.size());
		assert(state.function_scopes.back().blocks.size() == 1);
		state.function_scopes.pop_back();
	}

	return result;
}

int64_t evaluate_expr(const mylang::$$Parsed &par, State &state)
{
	if (par.identifier == "expr")
	{
		const auto &expr = par;

		if (expr.group[0].identifier == "expr_function")
		{
			const auto &expr_function = expr.group[0];
			return evaluate_expr(expr_function, state);
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

		if (expr_primitive.group[0].identifier == "expr_call")
		{
			const auto &expr_call = expr_primitive.group[0];
			return evaluate_expr(expr_call, state);
		}
		else
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
	if (par.identifier == "expr_function")
	{
		const auto &expr_function = par;

		auto l = state.create_lambda([](const Lambda *lambda, const std::vector<int64_t> &args, State &state) -> int64_t {

			if (lambda->args.size() != args.size())
				throw 1;

			for (size_t i = 0; i < args.size(); ++i)
				*state.declare_or_get_variable(lambda->args[i]) = args[i];

			const auto *expr_block = lambda->expr_block;

			for (size_t i = 1; i < expr_block->group.size() - 1; ++i)
			{
				const auto &expr_block_g0 = expr_block->group[i];
				const auto &statement = expr_block_g0.group[0];
				evaluate_statement(statement, state);
			}

			int64_t last_return = state.last_return;
			state.last_return = 0;

			return last_return;
		});

		if (const auto *expr_function_$g0 = expr_function.find("expr_function_$g0"))
		{
			const auto &token_path = expr_function_$g0->group[0];
			l->args.push_back(token_path.flatten());

			for (size_t i = 1; i < expr_function_$g0->group.size(); ++i)
			{
				const auto &expr_function_$g0_$g0 = expr_function_$g0->group[i];
				const auto &token_path = expr_function_$g0_$g0.group[1];
				l->args.push_back(token_path.flatten());
			}
		}

		l->expr_block = expr_function.find("expr_block");
		assert(l->expr_block);

		return std::bit_cast<int64_t>(l);
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
	if (statement.group[0].identifier == "statement_return")
	{
		const auto &statement_return = statement.group[0];
		const auto &expr = statement_return.group[1];

		state.last_return = evaluate_expr(expr, state);
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
		auto &func = state.function_scopes.emplace_back();
		func.name = "<global>";
		func.blocks.emplace_back();
	}

	state.create_lambda("std.print", [](const Lambda *, const std::vector<int64_t> &args, State &state) -> int64_t {
		for (size_t i = 0; i < args.size(); ++i)
		{
			if (i != 0)
				std::cout << " ";

			std::cout << args[i];
		}

		std::cout << "\n";

		return 0;
	});

	for (size_t i = 0; i < root.group.size(); ++i)
	{
		const auto &root_$g0 = root.group[i];
		const auto &statement = root_$g0.group[0];
		evaluate_statement(statement, state);
	}

	assert(state.function_scopes.size() == 1);
	assert(state.function_scopes[0].blocks.size() == 1);
}


} // namespace vm

void mylang_main()
{
	std::string_view text;

if (0)
{
text = R"AAA(

a = 3;
b = 4;
c = 5;
d = 6;

std.print(a + b * c + d);
std.print(a + b * (c + d));
std.print((a + b) * c + d);
std.print((a + b) * (c + d));


)AAA";
}

if (0)
{
text = R"AAA(

myprint = std.print;
myprint(myprint);

)AAA";
}

if (1)
{
text = R"AAA(

my_mul_sum = |a, b, c| {
	my_mul = |a, b| { return a * b; };
	return my_mul(a, b) + c;
};

std.print(my_mul_sum(2, 3, 4));


)AAA";
}

	const char *s = text.data();
	const char *e = s + text.size();

	auto result = mylang::$parse_root(s, e);

	// std::cout << mylang::helpers::generate_graphviz(result.value());

	int b = 0;

	vm::evaluate_root(result.value());

	int a = 0;
}

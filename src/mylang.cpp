#include "pch.hpp"

#include "gen/mylang.hpp"

#include <iostream>
#include <cassert>
#include <bit>
#include <memory>
#include <unordered_set>
#include <ranges>

namespace vm
{

struct IValue;
struct State;
struct LambdaValue;

using ivalue_t = std::shared_ptr<IValue>;
using lambda_ptr_t = ivalue_t(*)(const LambdaValue *lambda, const std::vector<ivalue_t> &args, State &state);
using variables_t = std::unordered_map<std::string, std::shared_ptr<IValue>>;

enum class EValueType
{
	Int64,
	Dict,
	Lambda,
};

struct IValue
{
	EValueType type;
	IValue(EValueType _type) : type{_type} {}
	virtual ~IValue() {}
};

template <EValueType _TYPE>
struct IValueTyped : IValue
{
	IValueTyped() : IValue(_TYPE) {}
};

struct Int64Value : IValueTyped<EValueType::Int64>
{
	int64_t val = 0;
};

struct DictValue : IValueTyped<EValueType::Dict>
{
	variables_t fields;
};

struct LambdaValue : IValueTyped<EValueType::Lambda>
{
	lambda_ptr_t lambda_ptr = nullptr;
	const mylang::$$Parsed *expr_block = nullptr;
	std::vector<std::string> args;
};

using vint64_t = std::shared_ptr<Int64Value>;
using vdict_t = std::shared_ptr<DictValue>;
using vlambda_t = std::shared_ptr<LambdaValue>;

struct State
{
	struct StateBlock
	{
		variables_t variables;
	};

	struct StateFunction
	{
		std::string name;
		std::vector<StateBlock> blocks;
	};

	std::vector<StateFunction> function_scopes;
	ivalue_t last_return;

	vint64_t create_int64(int64_t val)
	{
		auto i = std::make_shared<Int64Value>();
		i->val = val;
		return i;
	}

	vlambda_t create_lambda(lambda_ptr_t lambda)
	{
		auto l = std::make_shared<LambdaValue>();
		l->lambda_ptr = lambda;
		return l;
	}

	vlambda_t create_lambda(std::initializer_list<std::string> path, lambda_ptr_t lambda)
	{
		assert(path.size() > 0);

		auto l = create_lambda(lambda);

		auto it = path.begin();

		ivalue_t *v = &get_or_declare_variable(*it++);

		while (it != path.end())
		{
			if (!*v)
				*v = std::static_pointer_cast<IValue>(create_dict());

			v = &std::static_pointer_cast<DictValue>(*v)->fields[*it++];
		}

		*v = std::static_pointer_cast<IValue>(l);

		return l;
	}

	vdict_t create_dict()
	{
		auto d = std::make_shared<DictValue>();
		return d;
	}

	ivalue_t &get_or_declare_variable(const std::string &name)
	{
		assert(function_scopes.size() > 0);
		assert(function_scopes[0].blocks.size() > 0);

		return function_scopes.back().blocks.back().variables[name];
	}

	variables_t *get_current_variables()
	{
		assert(function_scopes.size() > 0);
		assert(function_scopes[0].blocks.size() > 0);

		return &function_scopes.back().blocks.back().variables;
	}

	ivalue_t find_variable(const std::string &name)
	{
		assert(function_scopes.size() > 0);
		assert(function_scopes[0].blocks.size() > 0);

		// ignore variables from caller functions
		auto &blocks = function_scopes.back().blocks;

		for (auto it = blocks.rbegin(); it != blocks.rend(); ++it)
		{
			if (auto v = it->variables.find(name); v != it->variables.end())
			{
				return v->second;
			}
		}

		return {};
	}
};

ivalue_t &evaluate_token_path(const mylang::$$Parsed &statement, State &state, bool declare);
void evaluate_statement(const mylang::$$Parsed &statement, State &state);

ivalue_t evaluate_call(ivalue_t func, const std::vector<ivalue_t> &args, State &state)
{
	if (!func)
		throw 1;

	if (func->type != EValueType::Lambda)
		throw 1;

	ivalue_t result;

	auto func_layer = state.function_scopes.size() + 1;

	{
		auto &func = state.function_scopes.emplace_back();
		func.blocks.emplace_back();
	}

	{
		auto l = std::static_pointer_cast<LambdaValue>(func);
		result = l->lambda_ptr(l.get(), args, state);
	}

	{
		assert(func_layer == state.function_scopes.size());
		assert(state.function_scopes.back().blocks.size() == 1);
		state.function_scopes.pop_back();
	}

	return result;
}

ivalue_t evaluate_expr(const mylang::$$Parsed &par, State &state)
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

		if (expr_add.group.size() > 1)
		{
			if (!result)
				throw 1;

			if (result->type != EValueType::Int64)
				throw 1;

			for (size_t i = 1; i < expr_add.group.size(); ++i)
			{
				const auto &expr_add_$g0 = expr_add.group[i];
				const auto &expr_add_$g0_$g0 = expr_add_$g0.group[0];

				const auto &sign = expr_add_$g0_$g0.group[0];
				const auto &expr_mul = expr_add_$g0.group[1];

				auto val = evaluate_expr(expr_mul, state);

				if (!val)
					throw 1;

				if (val->type != EValueType::Int64)
					throw 1;

				if (sign.literal == "+")
				{
					std::static_pointer_cast<Int64Value>(result)->val += std::static_pointer_cast<Int64Value>(val)->val;
				}
				else
				if (sign.literal == "-")
				{
					std::static_pointer_cast<Int64Value>(result)->val -= std::static_pointer_cast<Int64Value>(val)->val;
				}
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

		if (expr_mul.group.size() > 1)
		{
			if (!result)
				throw 1;

			if (result->type != EValueType::Int64)
				throw 1;

			for (size_t i = 1; i < expr_mul.group.size(); ++i)
			{
				const auto &expr_mul_$g0 = expr_mul.group[i];
				const auto &expr_mul_$g0_$g0 = expr_mul_$g0.group[0];

				const auto &sign = expr_mul_$g0_$g0.group[0];
				const auto &expr_primitive = expr_mul_$g0.group[1];

				auto val = evaluate_expr(expr_primitive, state);

				if (!val)
					throw 1;

				if (val->type != EValueType::Int64)
					throw 1;

				if (sign.literal == "*")
				{
					std::static_pointer_cast<Int64Value>(result)->val *= std::static_pointer_cast<Int64Value>(val)->val;
				}
				else
				if (sign.literal == "/")
				{
					std::static_pointer_cast<Int64Value>(result)->val /= std::static_pointer_cast<Int64Value>(val)->val;
				}
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
		return std::static_pointer_cast<IValue>(state.create_int64(atoi(number.flatten().c_str())));
	}
	else
	if (par.identifier == "token_path")
	{
		const auto &token_path = par;
		return evaluate_token_path(token_path, state, false);
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

		ivalue_t func;

		if (expr_call_$g0.group[0].identifier == "token_path")
		{
			const auto &token_path = expr_call_$g0.group[0];
			func = evaluate_token_path(token_path, state, false);
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

		std::vector<ivalue_t> args;

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

		return evaluate_call(func, args, state);
	}
	else
	if (par.identifier == "expr_function")
	{
		const auto &expr_function = par;

		auto l = state.create_lambda([](const LambdaValue *lambda, const std::vector<ivalue_t> &args, State &state) -> ivalue_t {

			if (lambda->args.size() != args.size())
				throw 1;

			for (size_t i = 0; i < args.size(); ++i)
				state.get_or_declare_variable(lambda->args[i]) = args[i];

			const auto *expr_block = lambda->expr_block;

			for (size_t i = 1; i < expr_block->group.size() - 1; ++i)
			{
				const auto &expr_block_g0 = expr_block->group[i];
				const auto &statement = expr_block_g0.group[0];
				evaluate_statement(statement, state);
			}

			ivalue_t last_return = state.last_return;
			state.last_return = {};

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

		return std::static_pointer_cast<IValue>(l);
	}
	else
	{
		throw 1;
	}
}

ivalue_t &evaluate_token_path(const mylang::$$Parsed &token_path, State &state, bool declare)
{
	const auto &token = token_path.group[0];

	auto *vars = state.get_current_variables();
	std::string tok = token.flatten();
	auto it = vars->find(tok);

	for (size_t i = 1; i < token_path.group.size(); ++i)
	{
		const auto &token_path_$g0 = token_path.group[i];
		const auto &token = token_path_$g0.group[1];

		if (it == vars->end())
			throw 1;

		assert(it->second);

		if (it->second->type != EValueType::Dict)
			throw 1;

		vars = &std::static_pointer_cast<DictValue>(it->second)->fields;
		tok = token.flatten();
		it = vars->find(tok);
	}

	if (it == vars->end())
	{
		if (!declare)
			throw 1;

		it = vars->insert({ tok, {} }).first;
	}

	return it->second;
}

void evaluate_statement(const mylang::$$Parsed &statement, State &state)
{
	if (statement.group[0].identifier == "statement_assign")
	{
		const auto &statement_assign = statement.group[0];
		const auto &token_path = statement_assign.group[0];
		const auto &expr = statement_assign.group[2];

		auto result = evaluate_expr(expr, state);
		evaluate_token_path(token_path, state, true) = result;
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

	state.create_lambda({"std","print"}, [](const LambdaValue *, const std::vector<ivalue_t> &args, State &state) -> ivalue_t {
		for (size_t i = 0; i < args.size(); ++i)
		{
			if (i != 0)
				std::cout << " ";

			if (args[i]->type == EValueType::Int64)
				std::cout << std::static_pointer_cast<Int64Value>(args[i])->val;
			else // TODO
				std::cout << "IValue{" << (void *)args[i].get() << "}";
		}

		std::cout << "\n";

		return {};
	});

	state.create_lambda({"std","dict"}, [](const LambdaValue *, const std::vector<ivalue_t> &args, State &state) -> ivalue_t {
		return state.create_dict();
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


dict = std.dict();

dict.std = std;

dict.my_mul = |dict, a, b| { return a * b; };

dict.my_mul_sum = |dict, a, b, c| {
	return dict.my_mul(dict, a, b) + c;
};

dict.my_mul_sum_print = |dict, a, b, c| {
	result = dict.my_mul_sum(dict, 2, 3, 4);
	dict.std.print(result);
};

dict.my_mul_sum_print(dict, 2, 3, 4);

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

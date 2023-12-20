#include "pch.hpp"

#include "gen/mylang.hpp"

#include <iostream>
#include <fstream>
#include <sstream>
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

enum class EValueType : uint8_t
{
	Bool,
	Str,
	I64,
	Dict,
	Lambda,
};

struct IValue
{
	EValueType type;
	bool const_value = false;
	IValue(EValueType _type) : type{_type} {}
	virtual ~IValue() {}
};

template <EValueType _TYPE>
struct IValueTyped : IValue
{
	IValueTyped() : IValue(_TYPE) {}
};

struct BoolValue : IValueTyped<EValueType::Bool>
{
	bool val = false;
};

struct StrValue : IValueTyped<EValueType::Str>
{
	std::string val;
};

struct I64Value : IValueTyped<EValueType::I64>
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
	variables_t captures;
};

using vbool_t = std::shared_ptr<BoolValue>;
using vi64_t = std::shared_ptr<I64Value>;
using vstr_t = std::shared_ptr<StrValue>;
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
		std::vector<std::unique_ptr<StateBlock>> blocks;
	};

	std::vector<StateFunction> function_scopes;
	ivalue_t last_return;

	vbool_t create_bool(bool val)
	{
		auto i = std::make_shared<BoolValue>();
		i->val = val;
		return i;
	}

	vstr_t create_str(std::string val)
	{
		auto s = std::make_shared<StrValue>();
		s->val = val;
		return s;
	}

	vi64_t create_i64(int64_t val)
	{
		auto i = std::make_shared<I64Value>();
		i->val = val;
		return i;
	}

	vlambda_t create_lambda(lambda_ptr_t lambda)
	{
		auto l = std::make_shared<LambdaValue>();
		l->lambda_ptr = lambda;
		return l;
	}

	vlambda_t create_static_lambda(std::initializer_list<std::string> path, lambda_ptr_t lambda)
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

	ivalue_t shallow_copy(ivalue_t v)
	{
		if (v->type == EValueType::Bool)
		{
			auto result = create_bool(false);
			*result = *std::static_pointer_cast<BoolValue>(v);
			return result;
		}
		else
		if (v->type == EValueType::Str)
		{
			auto result = create_str("");
			*result = *std::static_pointer_cast<StrValue>(v);
			return result;
		}
		else
		if (v->type == EValueType::I64)
		{
			auto result = create_i64(0);
			*result = *std::static_pointer_cast<I64Value>(v);
			return result;
		}
		else
		if (v->type == EValueType::Dict)
		{
			auto result = create_dict();
			*result = *std::static_pointer_cast<DictValue>(v);
			return result;
		}
		else
		if (v->type == EValueType::Lambda)
		{
			auto result = create_lambda(nullptr);
			*result = *std::static_pointer_cast<LambdaValue>(v);
			return result;
		}
		else
		{
			throw 1;
		}
	}

	ivalue_t &get_or_declare_variable(const std::string &name)
	{
		assert(function_scopes.size() > 0);
		assert(function_scopes[0].blocks.size() > 0);

		return function_scopes.back().blocks.back()->variables[name];
	}

	std::pair<ivalue_t *, StateBlock *> find_variable_with_block(const std::string &name)
	{
		assert(function_scopes.size() > 0);
		assert(function_scopes[0].blocks.size() > 0);

		// ignore variables from caller functions
		auto &blocks = function_scopes.back().blocks;

		for (auto it = blocks.rbegin(); it != blocks.rend(); ++it)
		{
			if (auto v = (*it)->variables.find(name); v != (*it)->variables.end())
			{
				return { &v->second, &**it };
			}
		}

		return { {}, blocks.back().get() };
	}

	ivalue_t find_variable(const std::string &name)
	{
		if (auto v = find_variable_with_block(name).first)
			return *v;

		return {};
	}
};

[[nodiscard]]
ivalue_t evaluate_expr_block(const mylang::$$Parsed& expr_block, State& state, bool new_scope);
[[nodiscard]]
ivalue_t &evaluate_token_path(const mylang::$$Parsed &statement, State &state, bool assign);
void evaluate_statement(const mylang::$$Parsed &statement, State &state);

void collect_captures(const mylang::$$Parsed &par, State &state, variables_t &captures)
{
	if (par.identifier == "token_path")
	{
		const auto &token_path = par;
		const auto &token = token_path.group[0];
		std::string tok = token.flatten();

		captures[tok] = state.find_variable(tok);
	}
	else
	{
		for (const auto &v : par.group)
			collect_captures(v, state, captures);
	}
}

[[nodiscard]]
ivalue_t evaluate_call(ivalue_t func, const std::vector<ivalue_t> &args, State &state)
{
	if (!func)
		throw 1;

	if (func->type != EValueType::Lambda)
		throw 1;

	ivalue_t result;

	[[maybe_unused]]
	auto func_layer = state.function_scopes.size() + 1;

	{
		state.function_scopes.emplace_back();
	}

	{
		auto l = std::static_pointer_cast<LambdaValue>(func);
		result = l->lambda_ptr(l.get(), args, state);
	}

	{
		assert(state.function_scopes.size() == func_layer);
		assert(state.function_scopes.back().blocks.size() == 0);
		state.function_scopes.pop_back();
	}

	return result;
}

[[nodiscard]]
ivalue_t evaluate_expr(const mylang::$$Parsed &par, State &state)
{
	if (par.identifier == "expr")
	{
		const auto &expr = par;
		const auto &expr_bool_compare = expr.group[0];
		return evaluate_expr(expr_bool_compare, state);
	}
	else
	if (par.identifier == "expr_bool_compare")
	{
		const auto &expr_bool_compare = par;
		const auto &expr_add_left = expr_bool_compare.group[0];

		auto left = evaluate_expr(expr_add_left, state);

		if (expr_bool_compare.group.size() > 1)
		{
			const auto &expr_bool_compare_$g0 = expr_bool_compare.group[1];
			const auto &expr_bool_compare_$g0_$g1 = expr_bool_compare_$g0.group[1];
			const auto expr_add_right = expr_bool_compare_$g0.group[3];

			auto right = evaluate_expr(expr_add_right, state);

			std::string comparator_str = expr_bool_compare_$g0_$g1.flatten();

			const auto &get_numbers = [&]() -> std::pair<int64_t, int64_t> {
				if (!left)
					throw 1;

				if (left->type != EValueType::I64)
					throw 1;

				if (!right)
					throw 1;

				if (right->type != EValueType::I64)
					throw 1;

				auto ileft = std::static_pointer_cast<I64Value>(left)->val;
				auto iright = std::static_pointer_cast<I64Value>(right)->val;

				return { ileft, iright };
			};

			if (comparator_str == "<=")
			{
				auto [ileft, iright] = get_numbers();
				return state.create_bool(ileft <= iright);
			}
			else
			if (comparator_str == ">=")
			{
				auto [ileft, iright] = get_numbers();
				return state.create_bool(ileft >= iright);
			}
			else
			if (comparator_str == "<" )
			{
				auto [ileft, iright] = get_numbers();
				return state.create_bool(ileft < iright);
			}
			else
			if (comparator_str == ">")
			{
				auto [ileft, iright] = get_numbers();
				return state.create_bool(ileft >= iright);
			}
			else
			if (comparator_str == "==")
			{
				if (!left && !right)
					return state.create_bool(true);

				if (left->type != right->type)
					return state.create_bool(false);

				if (left->type == EValueType::Bool)
				{
					auto ileft = std::static_pointer_cast<BoolValue>(left)->val;
					auto iright = std::static_pointer_cast<BoolValue>(right)->val;
				
					return state.create_bool(ileft == iright);
				}
				else
				if (left->type == EValueType::Str)
				{
					auto ileft = std::static_pointer_cast<StrValue>(left)->val;
					auto iright = std::static_pointer_cast<StrValue>(right)->val;
				
					return state.create_bool(ileft == iright);
				}
				else
				if (left->type == EValueType::I64)
				{
					auto ileft = std::static_pointer_cast<I64Value>(left)->val;
					auto iright = std::static_pointer_cast<I64Value>(right)->val;
				
					return state.create_bool(ileft == iright);
				}
				else
				if (left->type == EValueType::Dict || left->type == EValueType::Lambda)
				{
					return state.create_bool(left.get() == right.get());
				}
				else
				{
					throw 1;
				}
			}
			else
			if (comparator_str == "!=")
			{
				if (!left != !right)
					return state.create_bool(true);

				if (left->type != right->type)
					return state.create_bool(true);

				if (left->type == EValueType::Bool)
				{
					auto ileft = std::static_pointer_cast<BoolValue>(left)->val;
					auto iright = std::static_pointer_cast<BoolValue>(right)->val;
				
					return state.create_bool(ileft != iright);
				}
				else
				if (left->type == EValueType::Str)
				{
					auto ileft = std::static_pointer_cast<StrValue>(left)->val;
					auto iright = std::static_pointer_cast<StrValue>(right)->val;
				
					return state.create_bool(ileft != iright);
				}
				else
				if (left->type == EValueType::I64)
				{
					auto ileft = std::static_pointer_cast<I64Value>(left)->val;
					auto iright = std::static_pointer_cast<I64Value>(right)->val;
				
					return state.create_bool(ileft != iright);
				}
				else
				if (left->type == EValueType::Dict || left->type == EValueType::Lambda)
				{
					return state.create_bool(left.get() != right.get());
				}
				else
				{
					throw 1;
				}
			}
			else
			{
				throw 1;
			}
		}

		return left;
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

			result = state.shallow_copy(result);

			for (size_t i = 1; i < expr_add.group.size(); ++i)
			{
				const auto &expr_add_$g0 = expr_add.group[i];
				const auto &expr_add_$g0_$g1 = expr_add_$g0.group[1];

				const auto &sign = expr_add_$g0_$g1.group[0];
				const auto &expr_mul = expr_add_$g0.group[3];

				auto val = evaluate_expr(expr_mul, state);

				if (!val)
					throw 1;

				if (result->type != val->type)
					throw 1;

				if (result->type == EValueType::I64)
				{
					if (sign.literal == "+")
					{
						std::static_pointer_cast<I64Value>(result)->val += std::static_pointer_cast<I64Value>(val)->val;
					}
					else
					if (sign.literal == "-")
					{
						std::static_pointer_cast<I64Value>(result)->val -= std::static_pointer_cast<I64Value>(val)->val;
					}
				}
				else
				if (result->type == EValueType::Str)
				{
					if (sign.literal == "+")
					{
						std::static_pointer_cast<StrValue>(result)->val += std::static_pointer_cast<StrValue>(val)->val;
					}
					else
					{
						throw 1;
					}
				}
				else
				{
					throw 1;
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

			if (result->type != EValueType::I64)
				throw 1;

			for (size_t i = 1; i < expr_mul.group.size(); ++i)
			{
				const auto &expr_mul_$g0 = expr_mul.group[i];
				const auto &expr_mul_$g0_$g1 = expr_mul_$g0.group[1];

				const auto &sign = expr_mul_$g0_$g1.group[0];
				const auto &expr_primitive = expr_mul_$g0.group[3];

				auto val = evaluate_expr(expr_primitive, state);

				if (!val)
					throw 1;

				if (val->type != EValueType::I64)
					throw 1;

				if (sign.literal == "*")
				{
					std::static_pointer_cast<I64Value>(result)->val *= std::static_pointer_cast<I64Value>(val)->val;
				}
				else
				if (sign.literal == "/")
				{
					std::static_pointer_cast<I64Value>(result)->val /= std::static_pointer_cast<I64Value>(val)->val;
				}
			}
		}

		return result;
	}
	else
	if (par.identifier == "expr_primitive")
	{
		const auto &expr_primitive = par;
		
		if (expr_primitive.group[0].identifier == "expr_if")
		{
			const auto &expr_if = expr_primitive.group[0];
			return evaluate_expr(expr_if, state);
		}
		else
		if (expr_primitive.group[0].identifier == "expr_call")
		{
			const auto &expr_call = expr_primitive.group[0];
			return evaluate_expr(expr_call, state);
		}
		else
		if (expr_primitive.group[0].identifier == "expr_function")
		{
			const auto &expr_function = expr_primitive.group[0];
			return evaluate_expr(expr_function, state);
		}
		else
		if (expr_primitive.group[0].identifier == "kv_boolean")
		{
			const auto &kv_boolean = expr_primitive.group[0];
			return evaluate_expr(kv_boolean, state);
		}
		else
		if (expr_primitive.group[0].identifier == "str")
		{
			const auto &str = expr_primitive.group[0];
			return evaluate_expr(str, state);
		}
		else
		if (expr_primitive.group[0].identifier == "number")
		{
			const auto &number = expr_primitive.group[0];
			return evaluate_expr(number, state);
		}
		else
		if (expr_primitive.group[0].identifier == "expr_assign")
		{
			const auto &expr_assign = expr_primitive.group[0];
			return evaluate_expr(expr_assign, state);
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
	if (par.identifier == "kv_boolean")
	{
		const auto &kv_boolean = par;
		if (kv_boolean.group[0].identifier == "kv_false")
		{
			return state.create_bool(false);
		}
		else
		if (kv_boolean.group[0].identifier == "kv_true")
		{
			return state.create_bool(true);
		}
		else
		{
			throw 1;
		}
	}
	else
	if (par.identifier == "str")
	{
		const auto &str = par;
		const auto &str_$g0 = str.group[1];
		return state.create_str(str_$g0.flatten());
	}
	else
	if (par.identifier == "number")
	{
		const auto &number = par;
		return state.create_i64(atoll(number.flatten().c_str()));
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
		const auto &expr = expr_group.group[2];
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
		if (expr_call_$g0.group[0].identifier == "expr_group")
		{
			const auto &expr_group = expr_call_$g0.group[0];
			func = evaluate_expr(expr_group, state);
		}
		else
		if (expr_call_$g0.group[0].identifier == "expr_function")
		{
			const auto &expr_function = expr_call_$g0.group[0];
			func = evaluate_expr(expr_function, state);
		}
		else
		{
			throw 1;
		}

		std::vector<ivalue_t> args;

		if (const auto *expr_call_$g3 = expr_call.find("expr_call_$g3"))
		{
			const auto &expr = expr_call_$g3->group[0];
			args.push_back(evaluate_expr(expr, state));

			for (size_t i = 1; i < expr_call_$g3->group.size(); ++i)
			{
				if (expr_call_$g3->group[i].identifier == "expr_call_$g3_$g1")
				{
					const auto &expr_call_$g3_$g1 = expr_call_$g3->group[i];

					const auto &expr = expr_call_$g3_$g1.group[2];
					args.push_back(evaluate_expr(expr, state));
				}
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

			{
				assert(state.function_scopes.size() > 0);

				{
					auto &captures_block = state.function_scopes.back().blocks.emplace_back(std::make_unique<State::StateBlock>());

					captures_block->variables = lambda->captures;
				}

				{
					auto &arguments_block = state.function_scopes.back().blocks.emplace_back(std::make_unique<State::StateBlock>());

					for (size_t i = 0; i < args.size(); ++i)
						arguments_block->variables.insert({ lambda->args[i], args[i] });
				}
			}

			const auto *expr_block = lambda->expr_block;

			auto result = evaluate_expr_block(*expr_block, state, false);

			{
				assert(state.function_scopes.back().blocks.size() == 2);
				state.function_scopes.back().blocks.clear();
			}

			return result;
		});

		if (const auto *expr_function_$g1 = expr_function.find("expr_function_$g1"))
		{
			const auto &token = expr_function_$g1->group[0];
			l->args.push_back(token.flatten());

			for (size_t i = 1; i < expr_function_$g1->group.size(); ++i)
			{
				if (expr_function_$g1->group[i].identifier == "expr_function_$g1_$g1")
				{
					const auto &expr_function_$g1_$g1 = expr_function_$g1->group[i];
					const auto &token = expr_function_$g1_$g1.group[2];
					l->args.push_back(token.flatten());
				}
			}
		}

		l->expr_block = expr_function.find("expr_block");
		assert(l->expr_block);

		collect_captures(*l->expr_block, state, l->captures);

		return l;
	}
	else
	if (par.identifier == "expr_if")
	{
		const auto &expr_if = par;
		const auto &expr = expr_if.group[4];
		const auto &expr_block = expr_if.group[8];

		auto res = evaluate_expr(expr, state);

		if (res->type != EValueType::Bool)
			throw 1;

		auto bool_res = std::static_pointer_cast<BoolValue>(res)->val;

		if (bool_res)
		{
			return evaluate_expr(expr_block, state);
		}
		else
		{
			if (const auto *expr_if_$g4 = expr_if.find("expr_if_$g4"))
			{
				const auto &expr_block = expr_if_$g4->group[3];
				return evaluate_expr(expr_block, state);
			}
			else
			{
				return {};
			}
		}
	}
	else
	if (par.identifier == "expr_bool")
	{
		const auto &expr_bool = par;
		const auto &expr_bool_compare = expr_bool.group[0];
		return evaluate_expr(expr_bool_compare, state);
	}
	else
	if (par.identifier == "expr_block")
	{
		const auto& expr_block = par;
		return evaluate_expr_block(expr_block, state, true);
	}
	else
	if (par.identifier == "expr_assign")
	{
		const auto& expr_assign = par;
		const auto& token_path = expr_assign.group[0];
		const auto& expr = expr_assign.group[4];

		auto &res = evaluate_token_path(token_path, state, true);

		if (res && res->const_value)
			throw 1;

		return res = evaluate_expr(expr, state);
	}

	throw 1;
}

[[nodiscard]]
ivalue_t evaluate_expr_block(const mylang::$$Parsed &expr_block, State &state, bool new_scope)
{
	[[maybe_unused]]
	size_t scope_layer = 0;

	if (new_scope)
	{
		assert(state.function_scopes.size() > 0);
		state.function_scopes.back().blocks.emplace_back(std::make_unique<State::StateBlock>());
		scope_layer = state.function_scopes.back().blocks.size();
	}

	for (size_t i = 1; i < expr_block.group.size() - 1; ++i)
	{
		if (expr_block.group[i].identifier == "expr_block_$g1")
		{
			const auto& expr_block_g1 = expr_block.group[i];
			const auto& statement = expr_block_g1.group[0];
			evaluate_statement(statement, state);
		}
	}

	// TODO
	ivalue_t result = state.last_return;
	state.last_return = {};

	if (const auto* expr_block_$g2 = expr_block.find("expr_block_$g2"))
	{
		const auto& expr = expr_block_$g2->group[0];
		result = evaluate_expr(expr, state);
	}

	if (new_scope)
	{
		assert(state.function_scopes.size() > 0);
		assert(state.function_scopes.back().blocks.size() == scope_layer);
		state.function_scopes.back().blocks.pop_back();
	}

	return result;
}

[[nodiscard]]
ivalue_t &evaluate_token_path(const mylang::$$Parsed &token_path, State &state, bool assign)
{
	const auto &token = token_path.group[0];

	std::string tok = token.flatten();
	auto [val, block] = state.find_variable_with_block(tok);
	variables_t *vars = &block->variables;

	for (size_t i = 1; i < token_path.group.size(); ++i)
	{
		const auto &token_path_$g0 = token_path.group[i];
		const auto &token = token_path_$g0.group[3];

		if (!val)
			throw 1;

		if ((*val)->type != EValueType::Dict)
			throw 1;

		vars = &std::static_pointer_cast<DictValue>(*val)->fields;
		tok = token.flatten();

		if (auto it = vars->find(tok); it == vars->end())
			val = nullptr;
		else
			val = &it->second;
	}

	if (!val)
	{
		if (!assign || token_path.group.size() == 1)
			throw 1;

		val = &vars->insert({ tok, {} }).first->second;
	}

	return *val;
}

void evaluate_statement(const mylang::$$Parsed &statement, State &state)
{
	if (statement.group[0].identifier == "statement_let")
	{
		const auto &statement_assign = statement.group[0];
		const auto &statement_assign_$g1 = statement_assign.group[2];
		const auto &token = statement_assign.group[3];
		const auto &expr = statement_assign.group[7];

		assert(state.function_scopes.size() > 0);
		assert(state.function_scopes.back().blocks.size() > 0);

		if (auto insert_result = state.function_scopes.back().blocks.back()->variables.insert({ token.flatten(), {} }); insert_result.second)
		{
			ivalue_t &res = insert_result.first->second;
			res = evaluate_expr(expr, state);

			if (statement_assign_$g1.group.size() == 0 && res)
				res->const_value = true;
		}
		else
		{
			throw 1;
		}
	}
	else
	if (statement.group[0].identifier == "statement_return")
	{
		const auto &statement_return = statement.group[0];
		const auto &expr = statement_return.group[2];

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
	if (statement.group[0].identifier == "statement_if")
	{
		const auto &statement_if = statement.group[0];
		const auto &expr_if = statement_if.group[0];
		auto result = evaluate_expr(expr_if, state);
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
		func.blocks.emplace_back(std::make_unique<State::StateBlock>());
	}

	state.create_static_lambda({"std","print"}, [](const LambdaValue *, const std::vector<ivalue_t> &args, State &state) -> ivalue_t {
		for (size_t i = 0; i < args.size(); ++i)
		{
			if (i != 0)
				std::cout << " ";

			if (args[i]->type == EValueType::Bool)
				std::cout << (std::static_pointer_cast<BoolValue>(args[i])->val ? "true" : "false");
			else
			if (args[i]->type == EValueType::Str)
				std::cout << std::static_pointer_cast<StrValue>(args[i])->val;
			else
			if (args[i]->type == EValueType::I64)
				std::cout << std::static_pointer_cast<I64Value>(args[i])->val;
			else
				std::cout << "IValue{" << (void *)args[i].get() << "}";
		}

		std::cout << "\n";

		return {};
	});

	state.create_static_lambda({"std","bool"}, [](const LambdaValue *, const std::vector<ivalue_t> &args, State &state) -> ivalue_t {
		if (args.size() != 1)
			throw 1;

		if (args[0]->type == EValueType::Bool)
		{
			return state.create_bool(std::static_pointer_cast<BoolValue>(args[0])->val);
		}
		else
		if (args[0]->type == EValueType::Str)
		{
			// return state.create_bool(std::static_pointer_cast<StrValue>(args[0])->val.c_str());
			throw 1;
		}
		else
		if (args[0]->type == EValueType::I64)
		{
			return state.create_bool(std::static_pointer_cast<I64Value>(args[0])->val != 0);
		}
		else
		{
			throw 1;
		}
	});

	state.create_static_lambda({"std","str"}, [](const LambdaValue *, const std::vector<ivalue_t> &args, State &state) -> ivalue_t {
		if (args.size() != 1)
			throw 1;

		if (args[0]->type == EValueType::Bool)
		{
			return state.create_str(std::static_pointer_cast<BoolValue>(args[0]) ? "true" : "false");
		}
		else
		if (args[0]->type == EValueType::Str)
		{
			return state.create_str(std::static_pointer_cast<StrValue>(args[0])->val.c_str());
		}
		else
		if (args[0]->type == EValueType::I64)
		{
			return state.create_str(std::to_string(std::static_pointer_cast<I64Value>(args[0])->val));
		}
		else
		{
			throw 1;
		}
	});

	state.create_static_lambda({"std","i64"}, [](const LambdaValue *, const std::vector<ivalue_t> &args, State &state) -> ivalue_t {
		if (args.size() != 1)
			throw 1;

		if (args[0]->type == EValueType::Bool)
		{
			return state.create_i64(std::static_pointer_cast<BoolValue>(args[0]) ? 1 : 0);
		}
		else
		if (args[0]->type == EValueType::Str)
		{
			return state.create_i64(atoll(std::static_pointer_cast<StrValue>(args[0])->val.c_str()));
		}
		else
		if (args[0]->type == EValueType::I64)
		{
			return state.create_i64(std::static_pointer_cast<I64Value>(args[0])->val);
		}
		else
		{
			throw 1;
		}
	});

	state.create_static_lambda({"std","dict"}, [](const LambdaValue *, const std::vector<ivalue_t> &args, State &state) -> ivalue_t {
		return state.create_dict();
	});

	for (size_t i = 0; i < root.group.size(); ++i)
	{
		if (root.group[i].identifier == "root_$g1")
		{
			const auto &root_$g1 = root.group[i];
			const auto &statement = root_$g1.group[0];
			evaluate_statement(statement, state);
		}
	}

	assert(state.function_scopes.size() == 1);
	assert(state.function_scopes[0].blocks.size() == 1);
}


} // namespace vm

std::string read_file(const char *filename)
{
	std::ifstream f(filename);
	std::stringstream buffer;
	buffer << f.rdbuf();
	return buffer.str();
}

void mylang_main(int argc, const char **argv)
{
	std::string text = read_file("../src/mylang/mylang_test.txt");

	const char *s = text.data();
	const char *e = s + text.size();

	auto result = mylang::$parse_root(s, e);

	// std::cout << mylang::helpers::generate_graphviz(result.value());

	int b = 0;

	{
		std::unordered_map<std::string, std::string> colors;
		colors["token"] = "\033[96m";
		colors["str"] = "\033[91m";
		colors["number"] = "\033[92m";
		colors["kv_return"] = "\033[95m";
		colors["kv_let"] = "\033[94m";
		colors["kv_mut"] = "\033[94m";
		colors["kv_boolean"] = "\033[94m";
		colors["kv_if"] = "\033[95m";
		colors["kv_else"] = "\033[95m";
		std::cout << "--- code begin ---\n" << mylang::helpers::ansii_colored(result.value(), colors, "\033[0m") << "--- code end ---\n";
	}

	std::cout << "--- program begin ---\n";
	vm::evaluate_root(result.value());
	std::cout << "--- program end ---\n";

	int a = 0;
}

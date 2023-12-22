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


std::string replace_str(std::string str, const std::string& from, const std::string& to) {
	if (from.empty())
		return str;

	size_t start_pos = 0;

	while ((start_pos = str.find(from, start_pos)) != std::string::npos) {
		str.replace(start_pos, from.length(), to);
		start_pos += to.length(); // In case 'to' contains 'from', like replacing 'x' with 'yx'
	}

	return str;
}

std::string read_file(const char *filename)
{
	std::ifstream f(filename);
	std::stringstream buffer;
	buffer << f.rdbuf();
	return buffer.str();
}

template <typename T, size_t N>
struct allocator_with_buffer
{
	using value_type = T;

	template <class U>
	struct rebind { typedef allocator_with_buffer<U, N> other; };

	allocator_with_buffer() = default;

	explicit allocator_with_buffer(T *buffer) : _buffer{ buffer } {}

	template <class U>
	allocator_with_buffer(const allocator_with_buffer<U, N> &other) : _buffer{ (T *)other._buffer } {}

	T* _buffer;

	constexpr void deallocate(T *const _Ptr, const size_t _Count) {
#if _ITERATOR_DEBUG_LEVEL != 0
		if constexpr (std::is_same_v<T, std::_Container_proxy>)
		{
			return;
		}
#endif // _ITERATOR_DEBUG_LEVEL == 0

		if (_Ptr == _buffer)
			return;

		std::allocator<T>{}.deallocate(_Ptr, _Count);
	}

	[[nodiscard]] constexpr __declspec(allocator) T *allocate(const size_t _Count) {
#if _ITERATOR_DEBUG_LEVEL != 0
		if constexpr (std::is_same_v<T, std::_Container_proxy>)
		{
			return (T *)_buffer - 1; // _buffer_Container_proxy
		}
#endif // _ITERATOR_DEBUG_LEVEL == 0

		if (_Count <= N)
			return _buffer;

		return std::allocator<T>{}.allocate(_Count);
	}
};

template <typename T, size_t N>
struct small_vector : std::vector<T, allocator_with_buffer<T, N>>
{
#if _ITERATOR_DEBUG_LEVEL != 0
	alignas(std::_Container_proxy)
	char _buffer_Container_proxy[sizeof(std::_Container_proxy)];
#endif // _ITERATOR_DEBUG_LEVEL == 0

	alignas(T)
	char _buffer[N * sizeof(T)];

	using _base = std::vector<T, allocator_with_buffer<T, N>>;

	small_vector() : _base(allocator_with_buffer<T, N>((T *)_buffer))
	{
		_base::reserve(N);
	}

	small_vector(const small_vector &other) : small_vector()
	{
		std::copy(other.begin(), other.end(), std::back_inserter(*this));
	}

	small_vector &operator = (const small_vector &other)
	{
		_base::clear();
		std::copy(other.begin(), other.end(), std::back_inserter(*this));
		return *this;
	}

	small_vector(small_vector &&other) : small_vector()
	{
		std::move(other.begin(), other.end(), std::back_inserter(*this));
	}

	small_vector &operator = (small_vector &&other)
	{
		_base::clear();
		std::move(other.begin(), other.end(), std::back_inserter(*this));
		return *this;
	}
};



namespace vm
{

struct IValue;
struct State;
struct LambdaValue;
enum class EFlowChange;

constexpr size_t args_vector_soo_size = 4;

using ivalue_t = std::shared_ptr<IValue>;
using fivalue_t = std::pair<ivalue_t, EFlowChange>;
using args_vector = small_vector<ivalue_t, args_vector_soo_size>;
using lambda_ptr_t = fivalue_t(*)(const LambdaValue *lambda, const args_vector &args, State &state);


struct var_id_cache
{
	size_t struct_id = (size_t)-1;
	size_t var_index = (size_t)-1;
};

struct var_block_cache : var_id_cache
{
	size_t block_index = (size_t)-1;
};

struct variables_t
{
	small_vector<ivalue_t, 16> variables;
	size_t struct_id = 0;

	size_t find_variable_index(State &state, const std::string &v, var_id_cache &cache);
	size_t find_variable_index(State &state, const std::string &v);

	ivalue_t find_variable(State &state, const std::string &v, var_id_cache &cache);
	ivalue_t find_variable(State &state, const std::string &v);

	ivalue_t &get_or_declare_variable(State &state, const std::string &v, var_id_cache &cache);
	ivalue_t &get_or_declare_variable(State &state, const std::string &v);
};

[[maybe_unused]]
const int _EFlowChangeVersion = 4;

enum class EFlowChange
{
	None,
	Return,
	Continue,
	Break,
};

[[maybe_unused]]
const int _EValueTypeVersion = 5;

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
	size_t variables_struct_id = (size_t)-1;
};

struct LambdaValue : IValueTyped<EValueType::Lambda>
{
	lambda_ptr_t lambda_ptr = nullptr;
	const mylang::$$Parsed *expr_block = nullptr;
	small_vector<std::string, args_vector_soo_size> args;
	variables_t captures;
};

static_assert(_EValueTypeVersion == 5, "EValueType: check struct");

using vbool_t = std::shared_ptr<BoolValue>;
using vi64_t = std::shared_ptr<I64Value>;
using vstr_t = std::shared_ptr<StrValue>;
using vdict_t = std::shared_ptr<DictValue>;
using vlambda_t = std::shared_ptr<LambdaValue>;

static_assert(_EValueTypeVersion == 5, "EValueType: check shortcut");

#define flow_checked(var, expr) \
		auto _##var##_pair = expr; \
		if (_##var##_pair.second != vm::EFlowChange::None) \
			return _##var##_pair; \
		auto var = _##var##_pair.first

#define flow_unchanged(expr) \
	vm::fivalue_t{ expr, vm::EFlowChange::None }

template <typename TFunc>
struct ExitScope
{
	TFunc func;

	ExitScope(const TFunc &_func) : func(_func) {}
	~ExitScope() { func(); }
};

struct State
{
	struct StateBlock
	{
		variables_t variables;
	};

	struct StateFunction
	{
		std::string name;
		small_vector<std::shared_ptr<StateBlock>, 8> blocks;
		// TODO: unique_ptr
	};

	std::vector<StateFunction> function_scopes;
	std::vector<std::vector<std::string>> structs{ std::vector<std::string>() };

	int allow_continue = false;
	int allow_break = false;

	bool debug = false;

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

	ivalue_t &create_empty_static(std::initializer_list<std::string> path)
	{
		assert(path.size() > 0);

		auto it = path.begin();

		ivalue_t *v = &get_or_declare_variable(*it++);

		while (it != path.end())
		{
			if (!*v)
				*v = std::static_pointer_cast<IValue>(create_dict());

			v = &std::static_pointer_cast<DictValue>(*v)->fields.get_or_declare_variable(*this, *it++);
		}

		return *v;
	}

	vdict_t create_dict()
	{
		auto d = std::make_shared<DictValue>();
		return d;
	}

	static_assert(_EValueTypeVersion == 5, "EValueType: check create_");

	ivalue_t refval_copy(ivalue_t v)
	{
		static_assert(_EValueTypeVersion == 5, "EValueType: check refval_copy");

		if (!v)
		{
			return v;
		}
		else
		if (v->type == EValueType::Bool)
		{
			auto result = create_bool(false);
			result->val = std::static_pointer_cast<BoolValue>(v)->val;
			return result;
		}
		else
		if (v->type == EValueType::Str)
		{
			auto result = create_str("");
			result->val = std::static_pointer_cast<StrValue>(v)->val;
			return result;
		}
		else
		if (v->type == EValueType::I64)
		{
			auto result = create_i64(0);
			result->val = std::static_pointer_cast<I64Value>(v)->val;
			return result;
		}
		else
		if (v->type == EValueType::Dict || v->type == EValueType::Lambda)
		{
			return v;
		}
		else
		{
			throw 1;
		}
	}

	ivalue_t shallow_copy(ivalue_t v)
	{
		static_assert(_EValueTypeVersion == 5, "EValueType: check shallow_copy");

		if (!v)
		{
			return v;
		}
		else
		if (v->type == EValueType::Bool)
		{
			auto result = create_bool(false);
			result->val = std::static_pointer_cast<BoolValue>(v)->val;
			return result;
		}
		else
		if (v->type == EValueType::Str)
		{
			auto result = create_str("");
			result->val = std::static_pointer_cast<StrValue>(v)->val;
			return result;
		}
		else
		if (v->type == EValueType::I64)
		{
			auto result = create_i64(0);
			result->val = std::static_pointer_cast<I64Value>(v)->val;
			return result;
		}
		else
		if (v->type == EValueType::Dict)
		{
			auto result = create_dict();
			result->fields = std::static_pointer_cast<DictValue>(v)->fields;
			return result;
		}
		else
		if (v->type == EValueType::Lambda)
		{
			auto result = create_lambda(nullptr);
			result->lambda_ptr = std::static_pointer_cast<LambdaValue>(v)->lambda_ptr;
			result->expr_block = std::static_pointer_cast<LambdaValue>(v)->expr_block;
			result->args = std::static_pointer_cast<LambdaValue>(v)->args;
			result->captures = std::static_pointer_cast<LambdaValue>(v)->captures;
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

		return function_scopes.back().blocks.back()->variables.get_or_declare_variable(*this, name);
	}

	std::pair<ivalue_t *, StateBlock *> find_variable_with_block(const std::string &name, var_block_cache &cache)
	{
		assert(function_scopes.size() > 0);
		assert(function_scopes[0].blocks.size() > 0);

		// ignore variables from caller functions
		auto &blocks = function_scopes.back().blocks;

		if (cache.struct_id != (size_t)-1)
			return { &blocks[cache.block_index]->variables.variables[cache.var_index], &*blocks[cache.block_index] };

		size_t block_index = blocks.size() - 1;

		for (auto it = blocks.rbegin(); it != blocks.rend(); ++it, --block_index)
		{
			if (auto index = (*it)->variables.find_variable_index(*this, name, cache); index != (size_t)-1)
			{
				cache.block_index = block_index;
				return { &(*it)->variables.variables[index], &**it };
			}
		}

		return { {}, function_scopes.back().blocks.back().get() };
	}

	std::pair<ivalue_t *, StateBlock *> find_variable_with_block(const std::string &name)
	{
		var_block_cache cache;
		return find_variable_with_block(name, cache);
	}

	ivalue_t find_variable(const std::string &name, var_block_cache &cache)
	{
		if (auto v = find_variable_with_block(name, cache).first)
			return *v;

		return {};
	}

	ivalue_t find_variable(const std::string &name)
	{
		var_block_cache cache;
		return find_variable(name, cache);
	}

	ivalue_t find_variable(std::initializer_list<std::string> path, std::initializer_list<var_block_cache> cache)
	{
		assert(function_scopes.size() > 0);
		assert(function_scopes[0].blocks.size() > 0);
		assert(path.size() > 0);
		assert(cache.size() == path.size());

		auto it = path.begin();
		auto cit = cache.begin();

		auto v = find_variable(*it++, (var_block_cache &)*cit++);

		for (; it != path.end(); ++it, ++cit)
		{
			auto &vars = std::static_pointer_cast<DictValue>(v)->fields;

			if (auto vr = vars.find_variable(*this, *it, (var_block_cache &)*cit))
			{
				v = vr;
			}
			else
			{
				throw 1;
			}
		}

		return v;
	}

	ivalue_t find_variable(std::initializer_list<std::string> path)
	{
		assert(function_scopes.size() > 0);
		assert(function_scopes[0].blocks.size() > 0);
		assert(path.size() > 0);

		auto it = path.begin();

		auto v = find_variable(*it++);

		for (; it != path.end(); ++it)
		{
			auto &vars = std::static_pointer_cast<DictValue>(v)->fields;

			if (auto vr = vars.find_variable(*this, *it))
			{
				v = vr;
			}
			else
			{
				throw 1;
			}
		}

		return v;
	}
};


size_t variables_t::find_variable_index(State &state, const std::string &v, var_id_cache &cache)
{
	if (struct_id == cache.struct_id)
		return cache.var_index;

	for (size_t i = 0; i < state.structs[struct_id].size(); ++i)
	{
		if (state.structs[struct_id][i] == v)
		{
			return i;
		}
	}

	return (size_t)-1;
}

size_t variables_t::find_variable_index(State &state, const std::string &v)
{
	var_id_cache cache;
	return find_variable_index(state, v, cache);
}

ivalue_t variables_t::find_variable(State &state, const std::string &v, var_id_cache &cache)
{
	if (struct_id == cache.struct_id)
		return variables[cache.var_index];

	if (auto index = find_variable_index(state, v); index != (size_t)-1)
		return variables[index];

	return {};
}

ivalue_t variables_t::find_variable(State &state, const std::string &v)
{
	var_id_cache cache;
	return find_variable(state, v, cache);
}

ivalue_t &variables_t::get_or_declare_variable(State &state, const std::string &v, var_id_cache &cache)
{
	if (cache.struct_id != (size_t)-1)
	{
		if (struct_id == cache.struct_id)
			return variables[cache.var_index];

		assert(state.structs[cache.struct_id].size() == state.structs[struct_id].size() + 1);

		struct_id = cache.struct_id;

		return *variables.insert(variables.begin() + cache.var_index, ivalue_t{});
	}

	for (size_t i = 0; i < state.structs[struct_id].size(); ++i)
	{
		if (state.structs[struct_id][i] == v)
		{
			cache.struct_id = struct_id;
			cache.var_index = i;

			return variables[i];
		}
	}

	for (size_t j = 1; j < state.structs.size(); ++j)
	{
		if (j == struct_id)
			continue;

		if (state.structs[j].size() != state.structs[struct_id].size() + 1)
			continue;

		bool found = true;
		size_t found_i = (size_t)-1;

		for (size_t il = 0, ir = 0; ir < state.structs[struct_id].size(); ++il, ++ir)
		{
			if (state.structs[j][il] != state.structs[struct_id][ir])
			{
				if (state.structs[j][il] == v)
				{
					assert(found_i == (size_t)-1);

					found_i = il;
					--ir;

					continue;
				}

				found = false;
				break;
			}
		}

		if (found && found_i == (size_t)-1)
			if (state.structs[j].back() == v)
				found_i = state.structs[j].size() - 1;

		if (found && found_i != (size_t)-1)
		{
			struct_id = j;

			cache.struct_id = struct_id;
			cache.var_index = found_i;

			return *variables.insert(variables.begin() + found_i, ivalue_t{});
		}
	}

	auto &new_struct = state.structs.emplace_back();
	size_t new_struct_id = state.structs.size() - 1;
	new_struct = state.structs[struct_id];
	struct_id = new_struct_id;

	auto diff = new_struct.insert(std::lower_bound(new_struct.begin(), new_struct.end(), v), v) - new_struct.begin();

	cache.struct_id = struct_id;
	cache.var_index = diff;

	return *variables.insert(variables.begin() + diff, ivalue_t{});
}

ivalue_t &variables_t::get_or_declare_variable(State &state, const std::string &v)
{
	var_id_cache cache;
	return get_or_declare_variable(state, v, cache);
}

[[nodiscard]]
fivalue_t evaluate_expr_block(const mylang::$$Parsed &expr_block, State &state, bool new_scope);
[[nodiscard]]
ivalue_t &evaluate_token_path(const mylang::$$Parsed &statement, State &state, bool assign);
[[nodiscard]]
fivalue_t evaluate_statement(const mylang::$$Parsed &statement, State &state);

void fix_tree(mylang::$$Parsed &par)
{
	if (par.identifier == mylang::$$IdentifierType::$i_str)
	{
		auto &str = par;
		auto &str_$g0 = str.group[1];

		str_$g0.literal = replace_str(str_$g0.flatten(), "\\\"", "\"");
		str_$g0.type = mylang::$$ParsedType::Literal;
		str_$g0.group.clear();
	}
	else
	{
		for (auto &v : par.group)
			fix_tree(v);
	}
}

void optimize_tree(mylang::$$Parsed &par)
{
	if (par.identifier == mylang::$$IdentifierType::$i_expr)
	{
		auto tmp = std::move(par.group[0]);
		par = std::move(tmp);
	}

	if (par.identifier == mylang::$$IdentifierType::$i_expr_bool_compare)
	{
		if (par.group.size() == 1)
		{
			auto tmp = std::move(par.group[0]);
			par = std::move(tmp);
		}
	}

	if (par.identifier == mylang::$$IdentifierType::$i_expr_add)
	{
		if (par.group.size() == 1)
		{
			auto tmp = std::move(par.group[0]);
			par = std::move(tmp);
		}
	}

	if (par.identifier == mylang::$$IdentifierType::$i_expr_mul)
	{
		if (par.group.size() == 1)
		{
			auto tmp = std::move(par.group[0]);
			par = std::move(tmp);
		}
	}

	if (par.identifier == mylang::$$IdentifierType::$i_expr_primitive)
	{
		if (par.group.size() == 1)
		{
			auto tmp = std::move(par.group[0]);
			par = std::move(tmp);
		}
	}

	if (par.identifier == mylang::$$IdentifierType::$i_expr_group)
	{
		auto tmp = std::move(par.group[2]);
		par = std::move(tmp);
	}
	
	if (par.identifier == mylang::$$IdentifierType::$i_token)
	{
		auto &token = par;
		token.literal = token.flatten();
		token.type = mylang::$$ParsedType::Literal;
		token.group.clear();
	}

	if (par.identifier == mylang::$$IdentifierType::$i_number)
	{
		auto &number = par;
		number.literal = number.flatten();
		number.type = mylang::$$ParsedType::Literal;
		number.group.clear();
	}

	for (auto &v : par.group)
		optimize_tree(v);
}

void collect_captures(const mylang::$$Parsed &par, State &state, variables_t &captures)
{
	if (par.identifier == mylang::$$IdentifierType::$i_token_path)
	{
		const auto &token_path = par;
		const auto &token = token_path.group[0];
		std::string tok = token.flatten();

		captures.get_or_declare_variable(state, tok) = state.find_variable(tok);
	}
	else
	{
		for (const auto &v : par.group)
			collect_captures(v, state, captures);
	}
}

[[nodiscard]]
fivalue_t evaluate_call(ivalue_t func, const args_vector &args, State &state)
{
	if (state.debug)
		std::cout << "[debug] " << "evaluate_call" << "\n";

	if (!func)
		throw 1;

	if (func->type != EValueType::Lambda)
		throw 1;

	auto save_allow_continue = state.allow_continue;
	auto save_allow_break = state.allow_break;

	state.allow_continue = 0;
	state.allow_break = 0;

	ExitScope _onexit2([&]() {
		state.allow_continue = save_allow_continue;
		state.allow_break = save_allow_break;
	});

	{
		auto l = std::static_pointer_cast<LambdaValue>(func);
		auto checked = l->lambda_ptr(l.get(), args, state);

		static_assert(_EFlowChangeVersion == 4, "EFlowChange: check expr_call");

		if (checked.second == EFlowChange::None)
		{
			return checked;
		}
		else
		if (checked.second == EFlowChange::Return)
		{
			return flow_unchanged(checked.first);
		}
		else
		{
			throw 1;
		}
	}
}

[[nodiscard]]
fivalue_t evaluate_expr(const mylang::$$Parsed &par, State &state)
{
	if (state.debug)
		std::cout << "[debug] " << "evaluate_expr: par = " << replace_str(par.flatten(), "\n", "") << "\n";

	if (par.identifier == mylang::$$IdentifierType::$i_expr)
	{
		const auto &expr = par;
		const auto &expr_bool_compare = expr.group[0];
		return evaluate_expr(expr_bool_compare, state);
	}
	else
	if (par.identifier == mylang::$$IdentifierType::$i_expr_bool_compare)
	{
		const auto &expr_bool_compare = par;
		const auto &expr_add_left = expr_bool_compare.group[0];

		flow_checked(left, evaluate_expr(expr_add_left, state));

		if (expr_bool_compare.group.size() > 1)
		{
			const auto &expr_bool_compare_$g0 = expr_bool_compare.group[1];
			const auto &expr_bool_compare_$g0_$g1 = expr_bool_compare_$g0.group[1];
			const auto &expr_add_right = expr_bool_compare_$g0.group[3];

			flow_checked(right, evaluate_expr(expr_add_right, state));

			std::string comparator_str = expr_bool_compare_$g0_$g1.flatten();

			const auto &get_numbers = [&]() -> std::pair<int64_t, int64_t> {
				static_assert(_EValueTypeVersion == 5, "EValueType: check compare - get_numbers");

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
				return flow_unchanged(state.create_bool(ileft <= iright));
			}
			else
			if (comparator_str == ">=")
			{
				auto [ileft, iright] = get_numbers();
				return flow_unchanged(state.create_bool(ileft >= iright));
			}
			else
			if (comparator_str == "<")
			{
				auto [ileft, iright] = get_numbers();
				return flow_unchanged(state.create_bool(ileft < iright));
			}
			else
			if (comparator_str == ">")
			{
				auto [ileft, iright] = get_numbers();
				return flow_unchanged(state.create_bool(ileft >= iright));
			}
			else
			if (comparator_str == "==")
			{
				if (!left || !right)
					return flow_unchanged(state.create_bool(false));

				if (left->type != right->type)
					return flow_unchanged(state.create_bool(false));

				static_assert(_EValueTypeVersion == 5, "EValueType: check compare - eq");

				if (left->type == EValueType::Bool)
				{
					auto ileft = std::static_pointer_cast<BoolValue>(left)->val;
					auto iright = std::static_pointer_cast<BoolValue>(right)->val;
				
					return flow_unchanged(state.create_bool(ileft == iright));
				}
				else
				if (left->type == EValueType::Str)
				{
					auto ileft = std::static_pointer_cast<StrValue>(left)->val;
					auto iright = std::static_pointer_cast<StrValue>(right)->val;
				
					return flow_unchanged(state.create_bool(ileft == iright));
				}
				else
				if (left->type == EValueType::I64)
				{
					auto ileft = std::static_pointer_cast<I64Value>(left)->val;
					auto iright = std::static_pointer_cast<I64Value>(right)->val;
				
					return flow_unchanged(state.create_bool(ileft == iright));
				}
				else
				if (left->type == EValueType::Dict || left->type == EValueType::Lambda)
				{
					return flow_unchanged(state.create_bool(left.get() == right.get()));
				}
				else
				{
					throw 1;
				}
			}
			else
			if (comparator_str == "!=")
			{
				if (!left || !right)
					return flow_unchanged(state.create_bool(false));

				if (left->type != right->type)
					return flow_unchanged(state.create_bool(true));

				static_assert(_EValueTypeVersion == 5, "EValueType: check compare - neq");

				if (left->type == EValueType::Bool)
				{
					auto ileft = std::static_pointer_cast<BoolValue>(left)->val;
					auto iright = std::static_pointer_cast<BoolValue>(right)->val;
				
					return flow_unchanged(state.create_bool(ileft != iright));
				}
				else
				if (left->type == EValueType::Str)
				{
					auto ileft = std::static_pointer_cast<StrValue>(left)->val;
					auto iright = std::static_pointer_cast<StrValue>(right)->val;
				
					return flow_unchanged(state.create_bool(ileft != iright));
				}
				else
				if (left->type == EValueType::I64)
				{
					auto ileft = std::static_pointer_cast<I64Value>(left)->val;
					auto iright = std::static_pointer_cast<I64Value>(right)->val;
				
					return flow_unchanged(state.create_bool(ileft != iright));
				}
				else
				if (left->type == EValueType::Dict || left->type == EValueType::Lambda)
				{
					return flow_unchanged(state.create_bool(left.get() != right.get()));
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

		return flow_unchanged(left);
	}
	else
	if (par.identifier == mylang::$$IdentifierType::$i_expr_add)
	{
		const auto &expr_add = par;
		const auto &expr_mul = expr_add.group[0];

		flow_checked(left, evaluate_expr(expr_mul, state));

		if (expr_add.group.size() > 1)
		{
			static_assert(_EValueTypeVersion == 5, "EValueType: check expr_add left");

			if (!left)
				throw 1;

			left = state.shallow_copy(left);

			for (size_t i = 1; i < expr_add.group.size(); ++i)
			{
				const auto &expr_add_$g0 = expr_add.group[i];
				const auto &expr_add_$g0_$g1 = expr_add_$g0.group[1];

				const auto &sign = expr_add_$g0_$g1.group[0];
				const auto &expr_mul = expr_add_$g0.group[3];

				flow_checked(right, evaluate_expr(expr_mul, state));

				if (!right)
					throw 1;

				if (left->type != right->type)
					throw 1;

				static_assert(_EValueTypeVersion == 5, "EValueType: check expr_add right");

				if (left->type == EValueType::I64)
				{
					if (sign.literal == "+")
					{
						std::static_pointer_cast<I64Value>(left)->val += std::static_pointer_cast<I64Value>(right)->val;
					}
					else
					if (sign.literal == "-")
					{
						std::static_pointer_cast<I64Value>(left)->val -= std::static_pointer_cast<I64Value>(right)->val;
					}
				}
				else
				if (left->type == EValueType::Str)
				{
					if (sign.literal == "+")
					{
						std::static_pointer_cast<StrValue>(left)->val += std::static_pointer_cast<StrValue>(right)->val;
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

		return flow_unchanged(left);
	}
	else
	if (par.identifier == mylang::$$IdentifierType::$i_expr_mul)
	{
		const auto &expr_mul = par;
		const auto &expr_primitive = expr_mul.group[0];

		flow_checked(left, evaluate_expr(expr_primitive, state));

		if (expr_mul.group.size() > 1)
		{
			static_assert(_EValueTypeVersion == 5, "EValueType: check expr_mul left");

			if (!left)
				throw 1;

			if (left->type != EValueType::I64)
				throw 1;

			for (size_t i = 1; i < expr_mul.group.size(); ++i)
			{
				const auto &expr_mul_$g0 = expr_mul.group[i];
				const auto &expr_mul_$g0_$g1 = expr_mul_$g0.group[1];

				const auto &sign = expr_mul_$g0_$g1.group[0];
				const auto &expr_primitive = expr_mul_$g0.group[3];

				flow_checked(right, evaluate_expr(expr_primitive, state));

				if (!right)
					throw 1;

				if (right->type != EValueType::I64)
					throw 1;

				static_assert(_EValueTypeVersion == 5, "EValueType: check expr_mul right");

				if (sign.literal == "*")
				{
					std::static_pointer_cast<I64Value>(left)->val *= std::static_pointer_cast<I64Value>(right)->val;
				}
				else
				if (sign.literal == "/")
				{
					std::static_pointer_cast<I64Value>(left)->val /= std::static_pointer_cast<I64Value>(right)->val;
				}
				else
				if (sign.literal == "%")
				{
					std::static_pointer_cast<I64Value>(left)->val %= std::static_pointer_cast<I64Value>(right)->val;
				}
			}
		}

		return flow_unchanged(left);
	}
	else
	if (par.identifier == mylang::$$IdentifierType::$i_expr_primitive)
	{
		const auto &expr_primitive = par;
		
		if (expr_primitive.group[0].identifier == mylang::$$IdentifierType::$i_expr_if)
		{
			const auto &expr_if = expr_primitive.group[0];
			return evaluate_expr(expr_if, state);
		}
		else
		if (expr_primitive.group[0].identifier == mylang::$$IdentifierType::$i_expr_while)
		{
			const auto &expr_while = expr_primitive.group[0];
			return evaluate_expr(expr_while, state);
		}
		else
		if (expr_primitive.group[0].identifier == mylang::$$IdentifierType::$i_expr_call)
		{
			const auto &expr_call = expr_primitive.group[0];
			return evaluate_expr(expr_call, state);
		}
		else
		if (expr_primitive.group[0].identifier == mylang::$$IdentifierType::$i_expr_function)
		{
			const auto &expr_function = expr_primitive.group[0];
			return evaluate_expr(expr_function, state);
		}
		else
		if (expr_primitive.group[0].identifier == mylang::$$IdentifierType::$i_kv_boolean)
		{
			const auto &kv_boolean = expr_primitive.group[0];
			return evaluate_expr(kv_boolean, state);
		}
		else
		if (expr_primitive.group[0].identifier == mylang::$$IdentifierType::$i_str)
		{
			const auto &str = expr_primitive.group[0];
			return evaluate_expr(str, state);
		}
		else
		if (expr_primitive.group[0].identifier == mylang::$$IdentifierType::$i_number)
		{
			const auto &number = expr_primitive.group[0];
			return evaluate_expr(number, state);
		}
		else
		if (expr_primitive.group[0].identifier == mylang::$$IdentifierType::$i_expr_assign)
		{
			const auto &expr_assign = expr_primitive.group[0];
			return evaluate_expr(expr_assign, state);
		}
		else
		if (expr_primitive.group[0].identifier == mylang::$$IdentifierType::$i_token_path)
		{
			const auto &token_path = expr_primitive.group[0];
			return evaluate_expr(token_path, state);
		}
		else
		if (expr_primitive.group[0].identifier == mylang::$$IdentifierType::$i_expr_group)
		{
			const auto &expr_group = expr_primitive.group[0];
			return evaluate_expr(expr_group, state);
		}
		else
		if (expr_primitive.group[0].identifier == mylang::$$IdentifierType::$i_expr_block)
		{
			const auto &expr_block = expr_primitive.group[0];
			return evaluate_expr(expr_block, state);
		}
		else
		{
			throw 1;
		}
	}
	else
	if (par.identifier == mylang::$$IdentifierType::$i_kv_boolean)
	{
		const auto &kv_boolean = par;
		if (kv_boolean.group[0].identifier == mylang::$$IdentifierType::$i_kv_false)
		{
			return flow_unchanged(state.create_bool(false));
		}
		else
		if (kv_boolean.group[0].identifier == mylang::$$IdentifierType::$i_kv_true)
		{
			return flow_unchanged(state.create_bool(true));
		}
		else
		{
			throw 1;
		}
	}
	else
	if (par.identifier == mylang::$$IdentifierType::$i_str)
	{
		const auto &str = par;
		const auto &str_$g0 = str.group[1];
		return flow_unchanged(state.create_str(str_$g0.flatten()));
	}
	else
	if (par.identifier == mylang::$$IdentifierType::$i_number)
	{
		const auto &number = par;
		return flow_unchanged(state.create_i64(atoll(number.flatten().c_str())));
	}
	else
	if (par.identifier == mylang::$$IdentifierType::$i_token_path)
	{
		const auto &token_path = par;
		return flow_unchanged(evaluate_token_path(token_path, state, false));
	}
	else
	if (par.identifier == mylang::$$IdentifierType::$i_expr_group)
	{
		const auto &expr_group = par;
		const auto &expr = expr_group.group[2];
		return evaluate_expr(expr, state);
	}
	else
	if (par.identifier == mylang::$$IdentifierType::$i_expr_call)
	{
		const auto &expr_call = par;
		const auto &expr_call_$g0 = expr_call.group[0];

		ivalue_t func;

		if (expr_call_$g0.group[0].identifier == mylang::$$IdentifierType::$i_token_path)
		{
			const auto &token_path = expr_call_$g0.group[0];
			func = evaluate_token_path(token_path, state, false);
		}
		else
		if (expr_call_$g0.group[0].identifier == mylang::$$IdentifierType::$i_expr_group)
		{
			const auto &expr_group = expr_call_$g0.group[0];
			flow_checked(cheked, evaluate_expr(expr_group, state));
			func = cheked;
		}
		else
		if (expr_call_$g0.group[0].identifier == mylang::$$IdentifierType::$i_expr_function)
		{
			const auto &expr_function = expr_call_$g0.group[0];
			flow_checked(cheked, evaluate_expr(expr_function, state));
			func = cheked;
		}
		else
		{
			throw 1;
		}

		args_vector args;

		if (const auto *expr_call_$g3 = expr_call.find(mylang::$$IdentifierType::$i_expr_call_$g3))
		{
			const auto &expr = expr_call_$g3->group[0];
			flow_checked(cheked, evaluate_expr(expr, state));
			args.push_back(cheked);

			for (size_t i = 1; i < expr_call_$g3->group.size(); ++i)
			{
				if (expr_call_$g3->group[i].identifier == mylang::$$IdentifierType::$i_expr_call_$g3_$g1)
				{
					const auto &expr_call_$g3_$g1 = expr_call_$g3->group[i];

					const auto &expr = expr_call_$g3_$g1.group[2];
					flow_checked(cheked, evaluate_expr(expr, state));
					args.push_back(cheked);
				}
			}
		}

		return evaluate_call(func, args, state);
	}
	else
	if (par.identifier == mylang::$$IdentifierType::$i_expr_function)
	{
		const auto &expr_function = par;

		auto l = state.create_lambda([](const LambdaValue *lambda, const args_vector &args, State &state) -> fivalue_t {

			if (lambda->args.size() != args.size())
				throw 1;

			[[maybe_unused]]
			const auto func_layer = state.function_scopes.size() + 1;

			{
				auto &func = state.function_scopes.emplace_back();

				{
					auto &captures_block = func.blocks.emplace_back(std::make_shared<State::StateBlock>());

					captures_block->variables = lambda->captures;
				}

				{
					auto &arguments_block = func.blocks.emplace_back(std::make_shared<State::StateBlock>());

					for (size_t i = 0; i < args.size(); ++i)
						arguments_block->variables.get_or_declare_variable(state, lambda->args[i]) = args[i];
				}
			}

			ExitScope _onexit([&]() {
				{
					assert(state.function_scopes.back().blocks.size() == 2);
					state.function_scopes.back().blocks.clear();
				}

				{
					assert(state.function_scopes.size() == func_layer);
					assert(state.function_scopes.back().blocks.size() == 0);
					state.function_scopes.pop_back();
				}
			});

			const auto *expr_block = lambda->expr_block;

			auto result = evaluate_expr_block(*expr_block, state, false);

			return result;
		});

		if (const auto *expr_function_$g1 = expr_function.find(mylang::$$IdentifierType::$i_expr_function_$g1))
		{
			const auto &token = expr_function_$g1->group[0];
			l->args.push_back(token.flatten());

			for (size_t i = 1; i < expr_function_$g1->group.size(); ++i)
			{
				if (expr_function_$g1->group[i].identifier == mylang::$$IdentifierType::$i_expr_function_$g1_$g1)
				{
					const auto &expr_function_$g1_$g1 = expr_function_$g1->group[i];
					const auto &token = expr_function_$g1_$g1.group[2];
					l->args.push_back(token.flatten());
				}
			}
		}

		l->expr_block = expr_function.find(mylang::$$IdentifierType::$i_expr_block);
		assert(l->expr_block);

		collect_captures(*l->expr_block, state, l->captures);

		return flow_unchanged(l);
	}
	else
	if (par.identifier == mylang::$$IdentifierType::$i_expr_if)
	{
		const auto &expr_if = par;
		const auto &expr = expr_if.group[4];
		const auto &expr_block = expr_if.group[8];

		flow_checked(res, evaluate_expr(expr, state));

		if (res->type != EValueType::Bool)
			throw 1;

		auto bool_res = std::static_pointer_cast<BoolValue>(res)->val;

		if (bool_res)
		{
			return evaluate_expr(expr_block, state);
		}
		else
		{
			if (const auto *expr_if_$g4 = expr_if.find(mylang::$$IdentifierType::$i_expr_if_$g4))
			{
				const auto &expr_block = expr_if_$g4->group[3];
				return evaluate_expr(expr_block, state);
			}
			else
			{
				return flow_unchanged({});
			}
		}
	}
	else
	if (par.identifier == mylang::$$IdentifierType::$i_expr_while)
	{
		const auto &expr_while = par;
		const auto &expr = expr_while.group[4];
		const auto &expr_block = expr_while.group[8];

		bool bool_res = false;

		{
			auto save_allow_continue = state.allow_continue;
			auto save_allow_break = state.allow_break;

			state.allow_continue = 0;
			state.allow_break = 0;

			ExitScope _onexit([&]() {
				state.allow_continue = save_allow_continue;
				state.allow_break = save_allow_break;
			});

			flow_checked(res, evaluate_expr(expr, state));

			if (!res || res->type != EValueType::Bool)
				throw 1;

			bool_res = std::static_pointer_cast<BoolValue>(res)->val;
		}

		++state.allow_continue;
		++state.allow_break;

		ExitScope _onexit([&]() {
			--state.allow_continue;
			--state.allow_break;
		});

		while (bool_res)
		{
			static_assert(_EFlowChangeVersion == 4, "EFlowChange: check while block");

			auto checked = evaluate_expr(expr_block, state);

			if (checked.second == EFlowChange::None)
			{
				// intentionally empty
			}
			else
			if (checked.second == EFlowChange::Return)
			{
				return checked;
			}
			else
			if (checked.second == EFlowChange::Continue)
			{
				// intentionally empty
			}
			else
			if (checked.second == EFlowChange::Break)
			{
				return flow_unchanged(checked.first);
			}
			else
			{
				throw 1;
			}

			{
				auto save_allow_continue = state.allow_continue;
				auto save_allow_break = state.allow_break;

				state.allow_continue = 0;
				state.allow_break = 0;

				ExitScope _onexit([&]() {
					state.allow_continue = save_allow_continue;
					state.allow_break = save_allow_break;
				});

				flow_checked(res, evaluate_expr(expr, state));

				if (!res || res->type != EValueType::Bool)
					throw 1;

				bool_res = std::static_pointer_cast<BoolValue>(res)->val;
			}
		}

		if (const auto *expr_while_$g4 = expr_while.find(mylang::$$IdentifierType::$i_expr_while_$g4))
		{
			const auto &expr_block = expr_while_$g4->group[3];
			return evaluate_expr(expr_block, state);
		}
		else
		{
			return flow_unchanged({});
		}
	}
	else
	if (par.identifier == mylang::$$IdentifierType::$i_expr_block)
	{
		const auto &expr_block = par;
		return evaluate_expr_block(expr_block, state, true);
	}
	else
	if (par.identifier == mylang::$$IdentifierType::$i_expr_assign)
	{
		const auto &expr_assign = par;
		const auto &token_path = expr_assign.group[0];
		const auto &expr = expr_assign.group[4];

		auto &res = evaluate_token_path(token_path, state, true);

		auto aaa = token_path.flatten();

		if (res && res->const_value)
			throw 1;

		flow_checked(checked, evaluate_expr(expr, state));
		return flow_unchanged(res = state.refval_copy(checked));
	}

	throw 1;
}

[[nodiscard]]
fivalue_t evaluate_expr_block(const mylang::$$Parsed &expr_block, State &state, bool new_scope)
{
	if (state.debug)
		std::cout << "[debug] " << "evaluate_expr_block, expr_bloc = " << replace_str(expr_block.flatten(), "\n", "") << "\n";

	assert(expr_block.identifier == mylang::$$IdentifierType::$i_expr_block);

	[[maybe_unused]]
	size_t scope_layer = 0;

	if (new_scope)
	{
		assert(state.function_scopes.size() > 0);
		state.function_scopes.back().blocks.emplace_back(std::make_shared<State::StateBlock>());
		scope_layer = state.function_scopes.back().blocks.size();
	}

	ExitScope _onexit([&]() {
		if (new_scope)
		{
			assert(state.function_scopes.size() > 0);
			assert(state.function_scopes.back().blocks.size() == scope_layer);
			state.function_scopes.back().blocks.pop_back();
		}
	});

	for (size_t i = 1; i < expr_block.group.size() - 1; ++i)
	{
		if (expr_block.group[i].identifier == mylang::$$IdentifierType::$i_expr_block_$g1)
		{
			const auto &expr_block_g1 = expr_block.group[i];
			const auto &statement = expr_block_g1.group[0];
			flow_checked(checked, evaluate_statement(statement, state));
			(void)checked;
		}
	}

	if (const auto* expr_block_$g2 = expr_block.find(mylang::$$IdentifierType::$i_expr_block_$g2))
	{
		const auto &expr = expr_block_$g2->group[0];
		return evaluate_expr(expr, state);
	}

	return flow_unchanged({});
}

[[nodiscard]]
ivalue_t &evaluate_token_path(const mylang::$$Parsed &token_path, State &state, bool assign)
{
	if (state.debug)
		std::cout << "[debug] " << "evaluate_token_path: token_path = " << replace_str(token_path.flatten(), "\n", "") <<  "\n";

	assert(token_path.identifier == mylang::$$IdentifierType::$i_token_path);

#if 1

	struct TokenPathParsedCustomData : mylang::$$ParsedCustomData
	{
		size_t function_index = 0;
		size_t block_index = 0;

		std::vector<var_id_cache> var_ids;
	};

	if (token_path.custom_data)
	{
		TokenPathParsedCustomData &custom_data = *static_cast<TokenPathParsedCustomData *>(token_path.custom_data.get());

		variables_t *vars = &state.function_scopes[custom_data.function_index].blocks[custom_data.block_index]->variables;

		bool found = true;
		ivalue_t *val = nullptr;

		assert(custom_data.var_ids.size() > 0);

		for (size_t i = 0; i < custom_data.var_ids.size(); ++i)
		{
			if (vars->struct_id == custom_data.var_ids[i].struct_id)
			{
				val = &vars->variables[custom_data.var_ids[i].var_index];

				if (i + 1 != custom_data.var_ids.size())
				{
					if (!val)
						throw 1;

					if (!(*val))
						throw 1;

					if ((*val)->type != EValueType::Dict)
						throw 1;

					vars = &std::static_pointer_cast<DictValue>(*val)->fields;
				}
			}
			else
			{
				found = false;
				break;
			}
		}

		if (found)
			return *val;
	}

	const auto &token = token_path.group[0];

	std::string tok = token.flatten();

	{
		auto [val, block] = state.find_variable_with_block(tok);
		variables_t *vars = &block->variables;

		for (size_t i_f = 0; i_f != state.function_scopes.size(); ++i_f)
		{
			for (size_t i_b = 0; i_b != state.function_scopes[i_f].blocks.size(); ++i_b)
			{
				if (&block->variables == &state.function_scopes[i_f].blocks[i_b]->variables)
				{
					TokenPathParsedCustomData custom_data;

					custom_data.function_index = i_f;
					custom_data.block_index = i_b;

					if (val)
						custom_data.var_ids.push_back({ vars->struct_id, vars->find_variable_index(state, tok) });

					for (size_t i = 1; i < token_path.group.size(); ++i)
					{
						const auto &token_path_$g0 = token_path.group[i];
						const auto &token = token_path_$g0.group[3];

						if (!val)
							throw 1;

						if (!(*val))
							throw 1;

						if ((*val)->type != EValueType::Dict)
							throw 1;

						vars = &std::static_pointer_cast<DictValue>(*val)->fields;
						tok = token.flatten();

						if (auto index = vars->find_variable_index(state, tok); index == (size_t)-1)
						{
							val = nullptr;
						}
						else
						{
							val = &vars->variables[index];
							custom_data.var_ids.push_back({ vars->struct_id, index });
						}
					}

					if (!val || !*val)
					{
						if (!assign || token_path.group.size() == 1)
							throw 1;

						val = &vars->get_or_declare_variable(state, tok);

						custom_data.var_ids.push_back({ vars->struct_id, vars->find_variable_index(state, tok) });
					}

					token_path.custom_data = std::make_unique<TokenPathParsedCustomData>(std::move(custom_data));

					return *val;
				}
			}
		}
	}

	throw 1;
#else
	auto [val, block] = state.find_variable_with_block(tok);
	variables_t *vars = &block->variables;

	for (size_t i = 1; i < token_path.group.size(); ++i)
	{
		const auto &token_path_$g0 = token_path.group[i];
		const auto &token = token_path_$g0.group[3];

		if (!val)
			throw 1;

		if (!(*val))
			throw 1;

		if ((*val)->type != EValueType::Dict)
			throw 1;

		vars = &std::static_pointer_cast<DictValue>(*val)->fields;
		tok = token.flatten();

		if (auto index = vars->find_variable_index(state, tok); index == (size_t)-1)
			val = nullptr;
		else
			val = &vars->variables[index];
	}

	if (!val || !*val)
	{
		if (!assign || token_path.group.size() == 1)
			throw 1;

		val = &vars->get_or_declare_variable(state, tok);
	}

	return *val;
#endif
}

[[nodiscard]]
fivalue_t evaluate_statement(const mylang::$$Parsed &statement, State &state)
{
	if (state.debug)
		std::cout << "[debug] " << "evaluate_statement: statement = " << replace_str(statement.flatten(), "\n", "") <<  "\n";

	assert(statement.identifier == mylang::$$IdentifierType::$i_statement);

	if (statement.group[0].identifier == mylang::$$IdentifierType::$i_statement_if)
	{
		const auto &statement_if = statement.group[0];
		const auto &expr_if = statement_if.group[0];
		flow_checked(checked, evaluate_expr(expr_if, state));
		(void)checked;
		return flow_unchanged({});
	}
	else
	if (statement.group[0].identifier == mylang::$$IdentifierType::$i_statement_while)
	{
		const auto &statement_while = statement.group[0];
		const auto &expr_while = statement_while.group[0];
		flow_checked(checked, evaluate_expr(expr_while, state));
		(void)checked;
		return flow_unchanged({});
	}
	else
	if (statement.group[0].identifier == mylang::$$IdentifierType::$i_statement_block)
	{
		const auto &statement_block = statement.group[0];
		const auto &expr_block = statement_block.group[0];
		flow_checked(checked, evaluate_expr(expr_block, state));
		(void)checked;
		return flow_unchanged({});
	}
	else
	if (statement.group[0].identifier == mylang::$$IdentifierType::$i_statement_let)
	{
		const auto &statement_assign = statement.group[0];
		const auto &statement_assign_$g1 = statement_assign.group[2];
		const auto &token = statement_assign.group[3];
		const auto &expr = statement_assign.group[7];

		assert(state.function_scopes.size() > 0);
		assert(state.function_scopes.back().blocks.size() > 0);

		std::string tok = token.flatten();

		if (size_t index = state.function_scopes.back().blocks.back()->variables.find_variable_index(state, tok); index != (size_t)-1)
			throw 1;

		ivalue_t &res = state.function_scopes.back().blocks.back()->variables.get_or_declare_variable(state, tok);

		flow_checked(checked, evaluate_expr(expr, state));
		res = state.refval_copy(checked);

		if (statement_assign_$g1.group.size() == 0 && res)
			res->const_value = true;

		return flow_unchanged({});
	}
	else
	if (statement.group[0].identifier == mylang::$$IdentifierType::$i_statement_return)
	{
		const auto &statement_return = statement.group[0];

		if (auto statement_return_$g0 = statement_return.find(mylang::$$IdentifierType::$i_statement_return_$g0))
		{
			const auto &expr = statement_return_$g0->group[1];
			flow_checked(checked, evaluate_expr(expr, state));
			return fivalue_t{ checked, EFlowChange::Return };
		}
		else
		{
			return fivalue_t{ {}, EFlowChange::Return };
		}
	}
	else
	if (statement.group[0].identifier == mylang::$$IdentifierType::$i_statement_continue)
	{
		const auto &statement_continue = statement.group[0];
		(void)statement_continue;

		if (!state.allow_continue)
			throw 1;

		return fivalue_t{ {}, EFlowChange::Continue };
	}
	else
	if (statement.group[0].identifier == mylang::$$IdentifierType::$i_statement_break)
	{
		const auto &statement_break = statement.group[0];

		if (!state.allow_break)
			throw 1;

		if (auto statement_break_$g0 = statement_break.find(mylang::$$IdentifierType::$i_statement_break_$g0))
		{
			const auto &expr = statement_break_$g0->group[1];
			flow_checked(checked, evaluate_expr(expr, state));
			return fivalue_t{ checked, EFlowChange::Break };
		}
		else
		{
			return fivalue_t{ {}, EFlowChange::Break };
		}
	}
	else
	if (statement.group[0].identifier == mylang::$$IdentifierType::$i_statement_expr)
	{
		const auto &statement_expr = statement.group[0];
		const auto &expr = statement_expr.group[0];
		flow_checked(checked, evaluate_expr(expr, state));
		(void)checked;
		return flow_unchanged({});
	}
	else
	{
		throw 1;
	}
}

State prepare_state()
{
	State state;
	// state.debug = true;

	{
		auto &func = state.function_scopes.emplace_back();
		func.name = "<global>";
		func.blocks.emplace_back(std::make_shared<State::StateBlock>());
	}

	state.create_empty_static({"std","print"}) = state.create_lambda([](const LambdaValue *, const args_vector &args, State &state) -> fivalue_t {
		for (size_t i = 0; i < args.size(); ++i)
		{
			if (i != 0)
				std::cout << " ";

			static_assert(_EValueTypeVersion == 5, "EValueType: check std.print");

			if (!args[i])
				std::cout << "null";
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

		return flow_unchanged({});
	});

	state.create_empty_static({"std","bool"}) = state.create_lambda([](const LambdaValue *, const args_vector &args, State &state) -> fivalue_t {
		if (args.size() != 1)
			throw 1;

		static_assert(_EValueTypeVersion == 5, "EValueType: check std.bool");
		
		if (!args[0])
		{
			throw 1;
		}
		else
		if (args[0]->type == EValueType::Bool)
		{
			return flow_unchanged(state.create_bool(std::static_pointer_cast<BoolValue>(args[0])->val));
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
			return flow_unchanged(state.create_bool(std::static_pointer_cast<I64Value>(args[0])->val != 0));
		}
		else
		{
			throw 1;
		}
	});

	state.create_empty_static({"std","str"}) = state.create_lambda([](const LambdaValue *, const args_vector &args, State &state) -> fivalue_t {
		if (args.size() != 1)
			throw 1;

		static_assert(_EValueTypeVersion == 5, "EValueType: check std.str");
		
		if (!args[0])
		{
			throw 1;
		}
		else
		if (args[0]->type == EValueType::Bool)
		{
			return flow_unchanged(state.create_str(std::static_pointer_cast<BoolValue>(args[0]) ? "true" : "false"));
		}
		else
		if (args[0]->type == EValueType::Str)
		{
			return flow_unchanged(state.create_str(std::static_pointer_cast<StrValue>(args[0])->val.c_str()));
		}
		else
		if (args[0]->type == EValueType::I64)
		{
			return flow_unchanged(state.create_str(std::to_string(std::static_pointer_cast<I64Value>(args[0])->val)));
		}
		else
		{
			throw 1;
		}
	});

	state.create_empty_static({"std","i64"}) = state.create_lambda([](const LambdaValue *, const args_vector &args, State &state) -> fivalue_t {
		if (args.size() != 1)
			throw 1;

		static_assert(_EValueTypeVersion == 5, "EValueType: check std.i64");

		if (!args[0])
		{
			throw 1;
		}
		else
		if (args[0]->type == EValueType::Bool)
		{
			return flow_unchanged(state.create_i64(std::static_pointer_cast<BoolValue>(args[0]) ? 1 : 0));
		}
		else
		if (args[0]->type == EValueType::Str)
		{
			return flow_unchanged(state.create_i64(atoll(std::static_pointer_cast<StrValue>(args[0])->val.c_str())));
		}
		else
		if (args[0]->type == EValueType::I64)
		{
			return flow_unchanged(state.create_i64(std::static_pointer_cast<I64Value>(args[0])->val));
		}
		else
		{
			throw 1;
		}
	});

	state.create_empty_static({"std","dict"}) = state.create_lambda([](const LambdaValue *, const args_vector &args, State &state) -> fivalue_t {
		return flow_unchanged(state.create_dict());
	});

	state.create_empty_static({"std","null"}) = state.create_lambda([](const LambdaValue *, const args_vector &args, State &state) -> fivalue_t {
		return flow_unchanged({});
	});

	state.create_empty_static({"std","is_null"}) = state.create_lambda([](const LambdaValue *, const args_vector &args, State &state) -> fivalue_t {
		if (args.size() != 1)
			throw 1;

		return flow_unchanged(state.create_bool((bool)args[0]));
	});

	return state;
}

void run(State &state, const mylang::$$Parsed &root)
{
	assert(root.identifier == mylang::$$IdentifierType::$i_root);

	for (size_t i = 0; i < root.group.size(); ++i)
	{
		if (root.group[i].identifier == mylang::$$IdentifierType::$i_root_$g1)
		{
			const auto &root_$g1 = root.group[i];
			const auto &statement = root_$g1.group[0];

			auto checked = evaluate_statement(statement, state);

			if (checked.second != EFlowChange::None)
				throw 1;
		}
	}

	assert(state.function_scopes.size() == 1);
	assert(state.function_scopes[0].blocks.size() == 1);
}


} // namespace vm


void mylang_main(int argc, const char **argv)
{
	std::string text = read_file("../src/mylang/mylang_test.txt");

	const char *s = text.data();
	const char *e = s + text.size();

	auto root = mylang::$parse_root(s, e).value();

	// std::cout << mylang::helpers::generate_graphviz(root);

	int b = 0;

	{
		std::unordered_map<std::string, std::string> colors;
		colors["token"] = "\033[96m";
		colors["str"] = "\033[91m";
		colors["number"] = "\033[92m";
		colors["kv_return"] = "\033[95m";
		colors["kv_continue"] = "\033[95m";
		colors["kv_break"] = "\033[95m";
		colors["kv_let"] = "\033[94m";
		colors["kv_mut"] = "\033[94m";
		colors["kv_boolean"] = "\033[94m";
		colors["kv_if"] = "\033[95m";
		colors["kv_while"] = "\033[95m";
		colors["kv_else"] = "\033[95m";
		std::cout << "--- code begin ---\n" << mylang::helpers::ansii_colored(root, colors, "\033[0m") << "--- code end ---\n";
	}

	std::cout << "--- program begin ---\n";

	{
		vm::fix_tree(root);
		vm::optimize_tree(root);
		auto state = vm::prepare_state();
		vm::run(state, root);

		{
			auto &vars = state.function_scopes[0].blocks[0]->variables;

			if (auto on_user_update = vars.find_variable(state, "on_user_update"))
			{
				if (on_user_update->type != vm::EValueType::Lambda)
					throw 1;

				auto result = vm::evaluate_call(on_user_update, {}, state);
				(void)result;
			}
		}
	}

	std::cout << "--- program end ---\n";

	int a = 0;
}

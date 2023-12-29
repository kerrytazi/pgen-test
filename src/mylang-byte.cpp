#include "pch.hpp"

#include "gen/mylang-byte.hpp"

#include <iostream>
#include <fstream>
#include <sstream>
#include <cassert>
#include <memory>
#include <unordered_set>
#include <ranges>
#include <variant>
#include <algorithm>

#include "small_vector.hpp"
#include "sptr.hpp"

#ifndef NDEBUG
#define _comma_ ,
#define _debug_(v) v
#else
#define _comma_ ,
#define _debug_(v)
#endif



static std::unordered_map<std::string, std::string> ansii_colors {
	{ "token", "\033[96m" },
	{ "str", "\033[91m" },
	{ "number", "\033[92m" },
	{ "kv_return", "\033[95m" },
	{ "kv_continue", "\033[95m" },
	{ "kv_break", "\033[95m" },
	{ "kv_let", "\033[94m" },
	{ "kv_mut", "\033[94m" },
	{ "kv_boolean", "\033[94m" },
	{ "kv_if", "\033[95m" },
	{ "kv_while", "\033[95m" },
	{ "kv_else", "\033[95m" },
};
static std::string default_ansii_color = "\033[0m";


namespace util
{


char nibble_to_char(uint8_t nibble)
{
	const char table[16] { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };

	return table[nibble & 0xF];
};

std::string byte_to_str(uint8_t byte)
{
	std::string result(2, '0');

	result[1] = nibble_to_char(byte & 0xF);
	result[0] = nibble_to_char(byte >> 4);

	return result;
};

std::string bytes_to_str(const std::vector<uint8_t> &bytes)
{
	std::string result;

	for (uint8_t byte : bytes)
		result += byte_to_str(byte) + " ";

	if (result.size() > 0)
		result.resize(result.size() - 1);

	return result;
};

std::string num_to_str(size_t num)
{
	std::string result;

	for (size_t nibble_i = 0; nibble_i < 2 * sizeof(size_t); ++nibble_i)
	{
		char c = nibble_to_char(num >> (4 * nibble_i));

		if (c == '0' && result.size() > 0)
			break;

		result += c;
	}

	std::reverse(result.begin(), result.end());

	return result;
};

bool _is_whitespace(char c) {
	return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}
bool _is_not_whitespace(char c) {
	return !_is_whitespace(c);
}

std::string minimize_whitespaces(std::string str)
{
	auto it_begin = str.begin();

	while (true)
	{
		if (it_begin = std::find_if(it_begin, str.end(), _is_whitespace); it_begin != str.end())
		{
			auto it_end = std::find_if(it_begin, str.end(),  _is_not_whitespace);
			str.replace(it_begin, it_end, " ");
			++it_begin;

			continue;
		}

		break;
	}

	return str;
}

std::string replace_str(std::string str, const std::string& from, const std::string& to)
{
	if (from.empty())
		return str;

	size_t start_pos = 0;

	while ((start_pos = str.find(from, start_pos)) != std::string::npos)
	{
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

} // namespace util



namespace vm
{


void fix_tree(mylang::$Parsed &par)
{
	if (par.identifier == mylang::$IdentifierType::$i_str)
	{
		auto &str = par;
		auto &str_$g0 = str.group[1];

		str_$g0.literal = util::replace_str(str_$g0.flatten(), "\\\"", "\"");
		str_$g0.type = mylang::$ParsedType::Literal;
		str_$g0.group.clear();
	}
	else
	{
		for (auto &v : par.group)
			fix_tree(v);
	}
}

void optimize_tree(mylang::$Parsed &par)
{
	if (par.identifier == mylang::$IdentifierType::$i_statement)
	{
		auto tmp = std::move(par.group[0]);
		par = std::move(tmp);
	}

	if (par.identifier == mylang::$IdentifierType::$i_ws0)
	{
		auto &ws0 = par;
		ws0.literal = ws0.flatten();
		ws0.type = mylang::$ParsedType::Literal;
		ws0.group.clear();
	}

	if (par.identifier == mylang::$IdentifierType::$i_ws1)
	{
		auto &ws1 = par;
		ws1.literal = ws1.flatten();
		ws1.type = mylang::$ParsedType::Literal;
		ws1.group.clear();
	}

	if (par.identifier == mylang::$IdentifierType::$i_statement)
	{
		auto tmp = std::move(par.group[0]);
		par = std::move(tmp);
	}

	if (par.identifier == mylang::$IdentifierType::$i_expr)
	{
		auto tmp = std::move(par.group[0]);
		par = std::move(tmp);
	}

	if (par.identifier == mylang::$IdentifierType::$i_expr_compare)
	{
		if (par.size() == 1)
		{
			auto tmp = std::move(par.group[0]);
			par = std::move(tmp);
		}
	}

	if (par.identifier == mylang::$IdentifierType::$i_expr_add)
	{
		if (par.size() == 1)
		{
			auto tmp = std::move(par.group[0]);
			par = std::move(tmp);
		}
	}

	if (par.identifier == mylang::$IdentifierType::$i_expr_mul)
	{
		if (par.size() == 1)
		{
			auto tmp = std::move(par.group[0]);
			par = std::move(tmp);
		}
	}

	if (par.identifier == mylang::$IdentifierType::$i_expr_primitive)
	{
		if (par.size() == 1)
		{
			auto tmp = std::move(par.group[0]);
			par = std::move(tmp);
		}
	}

	if (par.identifier == mylang::$IdentifierType::$i_expr_group)
	{
		auto tmp = std::move(par.group[2]);
		par = std::move(tmp);
	}

	if (par.identifier == mylang::$IdentifierType::$i_token)
	{
		auto &token = par;
		token.literal = token.flatten();
		token.type = mylang::$ParsedType::Literal;
		token.group.clear();
	}

	if (par.identifier == mylang::$IdentifierType::$i_number)
	{
		auto &number = par;
		number.literal = number.flatten();
		number.type = mylang::$ParsedType::Literal;
		number.group.clear();
	}

	for (auto &v : par.group)
		optimize_tree(v);
}


template<class>
inline constexpr bool always_false_v = false;


template <typename TFunc>
struct ExitScope
{
	TFunc func;

	ExitScope(const TFunc &_func) : func(_func) {}
	~ExitScope() { func(); }
};



struct Program;
struct IRuntimeValueBase;
struct RuntimeValueLambda;

using ivalue_t = sptr<IRuntimeValueBase>;



enum class ERuntimeValueType
{
	I64,
	Str,
	Dict,
	Lambda,
};

struct IRuntimeValueBase
{
	ERuntimeValueType type;
	virtual ~IRuntimeValueBase() {}

	IRuntimeValueBase(ERuntimeValueType type_) : type{ type_ }
	{
	}
};

template <ERuntimeValueType _Type>
struct IRuntimeValueBaseTemplated : IRuntimeValueBase
{
	IRuntimeValueBaseTemplated() : IRuntimeValueBase(_Type)
	{
	}
};

struct RuntimeValueI64 : IRuntimeValueBaseTemplated<ERuntimeValueType::I64>
{
	int64_t val = 0;
	
	RuntimeValueI64() : IRuntimeValueBaseTemplated<ERuntimeValueType::I64>() {}

	RuntimeValueI64(int64_t val_) : RuntimeValueI64()
	{
		val = val_;
	}
};

struct RuntimeValueStr : IRuntimeValueBaseTemplated<ERuntimeValueType::Str>
{
	std::string val;
	
	RuntimeValueStr() : IRuntimeValueBaseTemplated<ERuntimeValueType::Str>() {}

	RuntimeValueStr(const std::string &val_) : RuntimeValueStr()
	{
		val = val_;
	}

	RuntimeValueStr(std::string &&val_) : RuntimeValueStr()
	{
		val = std::move(val_);
	}
};

struct RuntimeValueDict : IRuntimeValueBaseTemplated<ERuntimeValueType::Dict>
{
	std::unordered_map<std::string, ivalue_t> val;
	
	RuntimeValueDict() : IRuntimeValueBaseTemplated<ERuntimeValueType::Dict>() {}

	RuntimeValueDict(const std::unordered_map<std::string, ivalue_t> &val_) : RuntimeValueDict()
	{
		val = val_;
	}

	RuntimeValueDict(std::unordered_map<std::string, ivalue_t> &&val_) : RuntimeValueDict()
	{
		val = std::move(val_);
	}
};

struct FunctionInfo
{
	size_t code_offset = (size_t)-1;
	size_t stack_size = 0;
};

struct RuntimeValueLambda : IRuntimeValueBaseTemplated<ERuntimeValueType::Lambda>
{
	using lambda_t = std::function<void(Program &program, const std::span<ivalue_t> &args)>;

	lambda_t val;
	FunctionInfo func_info;
	small_vector<ivalue_t, 4> captures;

	RuntimeValueLambda() : IRuntimeValueBaseTemplated<ERuntimeValueType::Lambda>() {}

	RuntimeValueLambda(const lambda_t &val_) : RuntimeValueLambda()
	{
		val = val_;
	}

	RuntimeValueLambda(lambda_t &&val_) : RuntimeValueLambda()
	{
		val = std::move(val_);
	}
};

enum class StaticGlobalType : size_t
{
	RetVal,
	ResolveDict,
	Null,

	_size,
};

enum class InstructionOpCode : uint8_t
{
	NOP,

	RET,
	CALL,
	MOV,
	DGET,
	DSET,
	LC,
	ADD,

	CMP,

	JMP,
	JE,   // Zero,
	JNE,  // NotZero,
	JGT,  // Positive,
	JLE,  // NotPositive,
	JLT,  // Negative,
	JGE,  // NotNegative,

	HLT,
};

struct Program
{
	struct RetInfo
	{
		size_t code_offset;
		size_t stack_size;
	};

	std::vector<uint8_t> bytes;
	std::vector<RetInfo> ret_stack;
	std::vector<ivalue_t> stack;
	std::vector<ivalue_t> statics;

	FunctionInfo entry_point;

	enum class CmpResult
	{
		None,
		Equal,
		Less,
		Greater,
	};

	CmpResult last_cmp_result = CmpResult::None;

	/*
		number_1: 0000 0000 - 0111 1100
		number_2: 0111 1101 (then 2 bytes)
		number_4: 0111 1110 (then 4 bytes)
		number_8: 0111 1111 (then 8 bytes)
		static:   1xxx xxxx (same as number but upper bit is set to 1)
	*/
	std::pair<size_t, uint8_t> unpack_number(const uint8_t *&b)
	{
		uint8_t packed = *b++;
		uint8_t is_static = packed >> 7;
		packed &= (1 << 7) - 1;

		size_t id = 0;

		if (packed <= 0b0111'1100)
		{
			id = packed;
		}
		else
		if (packed == 0b0111'1101)
		{
			memcpy(&id, b, 2);
			b += 2;
		}
		else
		if (packed == 0b0111'1110)
		{
			memcpy(&id, b, 4);
			b += 4;
		}
		else
		{
			assert(packed == 0b0111'1111);
			memcpy(&id, b, 8);
			b += 8;
		}

		return { id, is_static };
	}

	ivalue_t &unpack_value(const std::pair<size_t, uint8_t> &number_pair, size_t stack_offset = 0)
	{
		auto [id, is_static] = number_pair;

		if (is_static)
			return statics[id];

		return *(stack.end() - (id + stack_offset) - 1);
	}

	ivalue_t &unpack_value(const uint8_t *&b, size_t stack_offset = 0)
	{
		return unpack_value(unpack_number(b), stack_offset);
	}

	ivalue_t run_call(const std::initializer_list<std::string> &path, const std::vector<ivalue_t> &args)
	{
		return run_call(*resolve(path)->casted<vm::RuntimeValueLambda>(), args);
	}

	ivalue_t run_call(const RuntimeValueLambda &lambda, const std::vector<ivalue_t> &args)
	{
		size_t prev_size = stack.size();
		size_t num_args = args.size();
		size_t num_captures = lambda.captures.size();
		size_t func_stack_size = lambda.func_info.stack_size;

		size_t func_total_stack_offset = num_args + num_captures + func_stack_size;

		stack.resize(prev_size + func_total_stack_offset);

		ExitScope _onexit([&]() {
			stack.resize(prev_size);
		});

		for (size_t i = 0; i < num_args; ++i)
			stack[prev_size + i] = args[i];
			// stack[prev_size + i] = unpack_value(b, func_total_stack_offset);

		for (size_t i = 0; i < num_captures; ++i)
			stack[prev_size + num_args + i] = lambda.captures[i];

		if (lambda.func_info.code_offset == (size_t)-1)
		{
			assert(lambda.val);
			assert(lambda.captures.size() == 0);
			assert(lambda.func_info.stack_size == 0);
			lambda.val(*this, std::span<ivalue_t>{ stack.data() + prev_size, num_args } );
		}
		else
		{
			ret_stack.push_back(RetInfo {
				//.code_offset = (size_t)(b - bytes.data()),
				.code_offset = (size_t)-1,
				.stack_size = prev_size,
			});

			// b = bytes.data() + lambda.func_info.code_offset;

			run(FunctionInfo {
				.code_offset = lambda.func_info.code_offset,
				.stack_size = stack.size(),
			});
		}

		return get_retval();
	}

	void run()
	{
		run(entry_point);
	}

	void run(const std::initializer_list<std::string> &path)
	{
		run(resolve(path)->casted<vm::RuntimeValueLambda>()->func_info);
	}

	void run(const FunctionInfo &func_info)
	{
		size_t prev_stack_size = stack.size();
		stack.resize(func_info.stack_size);
		ExitScope _onexit([&]() {
			stack.resize(prev_stack_size);
		});

		const uint8_t *b = bytes.data();

		b += func_info.code_offset;

		while (42)
		{
			std::cout << "[debug] pos: " << util::num_to_str((size_t)(b - bytes.data())) << "\n";
			InstructionOpCode opcode = (InstructionOpCode)*b++;

			switch (opcode)
			{
				case InstructionOpCode::NOP:
					break;


				case InstructionOpCode::RET:
					{
						auto &last_ret = ret_stack.back();

						b = bytes.data() + last_ret.code_offset;
						stack.resize(last_ret.stack_size);

						ret_stack.pop_back();

						if (last_ret.code_offset == (size_t)-1)
							return;

						break;
					}

				case InstructionOpCode::CALL:
					{
						const auto &func = *unpack_value(b);
						auto num_args_pair = unpack_number(b);

						assert(func.type == ERuntimeValueType::Lambda);
						assert(num_args_pair.second == 0);

						const auto &lambda = (RuntimeValueLambda &)func;

						size_t prev_size = stack.size();
						size_t num_args = num_args_pair.first;
						size_t num_captures = lambda.captures.size();
						size_t func_stack_size = lambda.func_info.stack_size;

						size_t func_total_stack_offset = num_args + num_captures + func_stack_size;

						stack.resize(prev_size + func_total_stack_offset);

						for (size_t i = 0; i < num_args; ++i)
							stack[prev_size + i] = unpack_value(b, func_total_stack_offset);

						for (size_t i = 0; i < num_captures; ++i)
							stack[prev_size + num_args + i] = lambda.captures[i];

						if (lambda.func_info.code_offset == (size_t)-1)
						{
							assert(lambda.val);
							assert(lambda.captures.size() == 0);
							assert(lambda.func_info.stack_size == 0);
							lambda.val(*this, std::span<ivalue_t>{ stack.data() + prev_size, num_args } );
							stack.resize(prev_size);
						}
						else
						{
							ret_stack.push_back(RetInfo {
								.code_offset = (size_t)(b - bytes.data()),
								.stack_size = prev_size,
							});

							b = bytes.data() + lambda.func_info.code_offset;
						}

						break;
					}

				case InstructionOpCode::MOV:
					{
						auto &dest = unpack_value(b);
						const auto &src = unpack_value(b);

						dest = src;
						break;
					}

				case InstructionOpCode::DGET:
					{
						auto &result = unpack_value(b);
						const auto &dict = unpack_value(b);
						const auto &path = unpack_value(b);

						assert(dict && dict->type == ERuntimeValueType::Dict);
						assert(path && path->type == ERuntimeValueType::Str);

						const auto &dict_val = dict.casted<RuntimeValueDict>()->val;
						auto it = dict.casted<RuntimeValueDict>()->val.find(path.casted<RuntimeValueStr>()->val);

						assert(it != dict_val.end());

						result = it->second;
						break;
					}

				case InstructionOpCode::DSET:
					{
						const auto &dict = unpack_value(b);
						const auto &path = unpack_value(b);
						const auto &val = unpack_value(b);

						assert(dict && dict->type == ERuntimeValueType::Dict);
						assert(path && path->type == ERuntimeValueType::Str);

						const auto &dict_val = dict.casted<RuntimeValueDict>()->val;
						auto it = dict.casted<RuntimeValueDict>()->val[path.casted<RuntimeValueStr>()->val] = val;

						break;
					}

				case InstructionOpCode::LC:
					{
						auto &result = unpack_value(b);
						const auto &func = unpack_value(b);
						auto num_captures_pair = unpack_number(b);

						assert(func->type == ERuntimeValueType::Lambda);
						assert(num_captures_pair.second == 0);
						size_t num_captures = num_captures_pair.first;

						const auto &lambda = func.casted<RuntimeValueLambda>();
						result = sptr<RuntimeValueLambda>::create_bounded(*lambda);

						for (size_t i = 0; i < num_captures; ++i)
							result.casted<RuntimeValueLambda>()->captures.push_back(unpack_value(b));

						break;
					}

				case InstructionOpCode::ADD:
					{
						auto &result = unpack_value(b);
						const auto &left = unpack_value(b);
						const auto &right = unpack_value(b);

						assert(left && left->type == ERuntimeValueType::I64);
						assert(right && right->type == ERuntimeValueType::I64);

						result = sptr<RuntimeValueI64>::create_bounded(left.casted<RuntimeValueI64>()->val + right.casted<RuntimeValueI64>()->val);
						break;
					}

				case InstructionOpCode::CMP:
					{
						const auto &left = unpack_value(b);
						const auto &right = unpack_value(b);

						assert(left && left->type == ERuntimeValueType::I64);
						assert(right && right->type == ERuntimeValueType::I64);

						last_cmp_result = CmpResult::None;

						if (left.casted<RuntimeValueI64>()->val < right.casted<RuntimeValueI64>()->val)
							last_cmp_result = CmpResult::Less;

						if (left.casted<RuntimeValueI64>()->val > right.casted<RuntimeValueI64>()->val)
							last_cmp_result = CmpResult::Greater;

						if (left.casted<RuntimeValueI64>()->val == right.casted<RuntimeValueI64>()->val)
							last_cmp_result = CmpResult::Equal;

						break;
					}

				case InstructionOpCode::JMP:
					{
						auto [code_offset, is_static] = unpack_number(b);
						(void)is_static;

						b = bytes.data() + code_offset;

						break;
					}

				case InstructionOpCode::JE:
					{
						auto [code_offset, is_static] = unpack_number(b);
						(void)is_static;

						if (last_cmp_result == CmpResult::Equal)
							b = bytes.data() + code_offset;

						break;
					}

				case InstructionOpCode::JNE:
					{
						auto [code_offset, is_static] = unpack_number(b);
						(void)is_static;

						if (last_cmp_result != CmpResult::Equal)
							b = bytes.data() + code_offset;

						break;
					}

				case InstructionOpCode::JGT:
					{
						auto [code_offset, is_static] = unpack_number(b);
						(void)is_static;

						if (last_cmp_result == CmpResult::Greater)
							b = bytes.data() + code_offset;

						break;
					}

				case InstructionOpCode::JLE:
					{
						auto [code_offset, is_static] = unpack_number(b);
						(void)is_static;

						if (last_cmp_result != CmpResult::Greater)
							b = bytes.data() + code_offset;

						break;
					}

				case InstructionOpCode::JLT:
					{
						auto [code_offset, is_static] = unpack_number(b);
						(void)is_static;

						if (last_cmp_result == CmpResult::Less)
							b = bytes.data() + code_offset;

						break;
					}

				case InstructionOpCode::JGE:
					{
						auto [code_offset, is_static] = unpack_number(b);
						(void)is_static;

						if (last_cmp_result != CmpResult::Less)
							b = bytes.data() + code_offset;

						break;
					}


				case InstructionOpCode::HLT:
					return;

				default:
					throw 1;
			}
		}
	}

	void set_retval(ivalue_t val)
	{
		statics[(size_t)StaticGlobalType::RetVal] = val;
	}

	ivalue_t get_retval()
	{
		return statics[(size_t)StaticGlobalType::RetVal];
	}

	ivalue_t *resolve(const std::initializer_list<std::string> &path)
	{
		ivalue_t *v = &statics[(size_t)StaticGlobalType::ResolveDict];

		for (const auto &tok : path)
		{
			if (!*v)
				*v = sptr<RuntimeValueDict>::create_bounded();

			v = &v->casted<RuntimeValueDict>()->val[tok];
		}

		return v;
	}
};


struct Compiler
{
	struct LocalVariableID
	{
		size_t id;

		std::string to_string_bytecode() const
		{
			return "l" + std::to_string(id);
		}
	};

	struct StaticVariableID
	{
		size_t id;

		std::string to_string_bytecode() const
		{
			return "s" + std::to_string(id);
		}
	};

	struct CaptureVariableID
	{
		size_t id;

		std::string to_string_bytecode() const
		{
			return "c" + std::to_string(id);
		}
	};

	struct ParameterVariableID
	{
		size_t id;

		std::string to_string_bytecode() const
		{
			return "p" + std::to_string(id);
		}
	};

	enum class ConditionType
	{
		Zero,
		NotZero,
		Positive,
		NotPositive,
		Negative,
		NotNegative,
	};

	struct CodePosMark
	{
		size_t id;

		std::string to_string_bytecode() const
		{
			return "mark" + std::to_string(id);
		}
	};

	using VarIDType_Base = std::variant<
		LocalVariableID,
		StaticVariableID,
		CaptureVariableID,
		ParameterVariableID,
		ConditionType,
		CodePosMark
	>;

	struct VarIDType : VarIDType_Base
	{
		template <typename T>
		VarIDType(const T &v) : VarIDType_Base(v)
		{
		}

		std::string to_string_number() const
		{
			const auto &visitor = [&](const auto &v) -> size_t {
				using T = std::decay_t<decltype(v)>;

				if constexpr (std::is_same_v<T, ConditionType>)
				{
					throw 1;
				}
				else
				{
					return v.id;
				}
			};

			return util::num_to_str(std::visit(visitor, *this));
		}

		std::string to_string_bytecode() const
		{
			const auto &visitor = [&](const auto &v) -> std::string {
				using T = std::decay_t<decltype(v)>;

				if constexpr (std::is_same_v<T, ConditionType>)
				{
					throw 1;
				}
				else
				{
					return v.to_string_bytecode();
				}
			};

			return std::visit(visitor, *this);
		}

		constexpr bool is_static_type(StaticGlobalType type) const
		{
			if (auto v_ret_static = std::get_if<StaticVariableID>(this))
			{
				if (v_ret_static->id == (size_t)type)
				{
					return true;
				}
			}

			return false;
		}

		constexpr bool is_retval() const
		{
			return is_static_type(StaticGlobalType::RetVal);
		}

		constexpr bool is_null() const
		{
			return is_static_type(StaticGlobalType::Null);
		}
	};

	struct FunctionBlock;

	struct LambdaInfo
	{
		FunctionBlock *function_block;
	};

	struct StaticNull
	{
	};

	using StaticType = std::variant<
		int64_t,
		std::string,
		LambdaInfo,
		StaticNull
	>;


	/*
		number_1: 0000 0000 - 0111 1100
		number_2: 0111 1101 (then 2 bytes)
		number_4: 0111 1110 (then 4 bytes)
		number_8: 0111 1111 (then 8 bytes)
		static:   1xxx xxxx (same as number but upper bit is set to 1)
	*/
	static void pack_number(std::vector<uint8_t> &bytecode, const VarIDType &v)
	{
		const auto &visitor = [&](const auto &v) -> size_t {
			using T = std::decay_t<decltype(v)>;

			if constexpr (std::is_same_v<T, ConditionType>)
			{
				throw 1;
			}
			else
			{
				return v.id;
			}
		};

		uint64_t id = std::visit(visitor, v);
		bool is_static = std::holds_alternative<StaticVariableID>(v);

		if (id <= (uint64_t)0b0111'1100)
		{
			uint8_t packed = (uint8_t)id;

			if (is_static)
				packed |= 1 << 7;

			bytecode.push_back(packed);
		}
		else
		if (id <= (uint64_t)std::numeric_limits<uint16_t>::max())
		{
			uint8_t packed = (uint8_t)0b0111'1101;
			uint16_t packed2 = (uint16_t)id;

			if (is_static)
				packed |= 1 << 7;

			bytecode.push_back(packed);
			bytecode.insert(bytecode.end(), (const uint8_t *)(&packed2), (const uint8_t *)(&packed2 + 1));
		}
		else
		if (id <= (uint64_t)std::numeric_limits<uint32_t>::max())
		{
			uint8_t packed = (uint8_t)0b0111'1110;
			uint32_t packed4 = (uint32_t)id;

			if (is_static)
				packed |= 1 << 7;

			bytecode.push_back(packed);
			bytecode.insert(bytecode.end(), (const uint8_t *)(&packed4), (const uint8_t *)(&packed4 + 1));
		}
		else
		{
			uint8_t packed = (uint8_t)0b0111'1111;
			uint32_t packed8 = (uint32_t)id;

			if (is_static)
				packed |= 1 << 7;

			bytecode.push_back(packed);
			bytecode.insert(bytecode.end(), (const uint8_t *)(&packed8), (const uint8_t *)(&packed8 + 1));
		}
	}

	struct Instruction_NOP
	{
		_debug_(std::string _debug_info;);

		void generate_bytecode(const FunctionBlock &funcblock, std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::NOP);
		}

		std::string to_string_bytecode() const
		{
			std::string res;

			res += "nop";

			return res;
		}
	};

	struct Instruction_RET
	{
		_debug_(std::string _debug_info;);

		void generate_bytecode(const FunctionBlock &funcblock, std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::RET);
		}

		std::string to_string_bytecode() const
		{
			std::string res;

			res += "ret";

			return res;
		}
	};

	struct Instruction_CALL
	{
		VarIDType func;
		std::vector<VarIDType> args;

		_debug_(std::string _debug_info;);

		void generate_bytecode(const FunctionBlock &funcblock, std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::CALL);
			pack_number(bytecode, funcblock.regenerate(func));
			pack_number(bytecode, LocalVariableID{ args.size() });

			for (const auto &arg : args)
				pack_number(bytecode, funcblock.regenerate(arg));
		}

		std::string to_string_bytecode() const
		{
			std::string res;

			res += "call ";
			res += func.to_string_bytecode() + " ";

			for (const auto &arg : args)
				res += arg.to_string_bytecode() + " ";

			res.resize(res.size() - 1);

			return res;
		}
	};

	struct Instruction_MOV
	{
		VarIDType dest;
		VarIDType src;

		_debug_(std::string _debug_info;);

		void generate_bytecode(const FunctionBlock &funcblock, std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::MOV);
			pack_number(bytecode, funcblock.regenerate(dest));
			pack_number(bytecode, funcblock.regenerate(src));
		}

		std::string to_string_bytecode() const
		{
			std::string res;

			res += "mov ";
			res += dest.to_string_bytecode();
			res += " ";
			res += src.to_string_bytecode();

			return res;
		}
	};

	struct Instruction_DGET
	{
		VarIDType result;
		VarIDType dict;
		VarIDType path;

		_debug_(std::string _debug_info;);

		void generate_bytecode(const FunctionBlock &funcblock, std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::DGET);
			pack_number(bytecode, funcblock.regenerate(result));
			pack_number(bytecode, funcblock.regenerate(dict));
			pack_number(bytecode, funcblock.regenerate(path));
		}

		std::string to_string_bytecode() const
		{
			std::string res;

			res += "dget ";
			res += result.to_string_bytecode();
			res += " ";
			res += dict.to_string_bytecode();
			res += " ";
			res += path.to_string_bytecode();

			return res;
		}
	};

	struct Instruction_DSET
	{
		VarIDType dict;
		VarIDType path;
		VarIDType val;

		_debug_(std::string _debug_info;);

		void generate_bytecode(const FunctionBlock &funcblock, std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::DSET);
			pack_number(bytecode, funcblock.regenerate(dict));
			pack_number(bytecode, funcblock.regenerate(path));
			pack_number(bytecode, funcblock.regenerate(val));
		}

		std::string to_string_bytecode() const
		{
			std::string res;

			res += "dset ";
			res += dict.to_string_bytecode();
			res += " ";
			res += path.to_string_bytecode();
			res += " ";
			res += val.to_string_bytecode();

			return res;
		}
	};

	struct Instruction_LC
	{
		VarIDType result;
		VarIDType func;
		std::vector<VarIDType> captures;

		_debug_(std::string _debug_info;);

		void generate_bytecode(const FunctionBlock &funcblock, std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::LC);
			pack_number(bytecode, funcblock.regenerate(result));
			pack_number(bytecode, funcblock.regenerate(func));
			pack_number(bytecode, LocalVariableID{ captures.size() });

			for (const auto &capture : captures)
				pack_number(bytecode, funcblock.regenerate(capture));
		}

		std::string to_string_bytecode() const
		{
			std::string res;

			res += "lc ";
			res += result.to_string_bytecode() + " ";
			res += func.to_string_bytecode() + " ";

			for (const auto &capture : captures)
				res += capture.to_string_bytecode() + " ";

			res.resize(res.size() - 1);

			return res;
		}
	};

	struct Instruction_ADD
	{
		VarIDType result;
		VarIDType left;
		VarIDType right;

		_debug_(std::string _debug_info;);

		void generate_bytecode(const FunctionBlock &funcblock, std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::ADD);
			pack_number(bytecode, funcblock.regenerate(result));
			pack_number(bytecode, funcblock.regenerate(left));
			pack_number(bytecode, funcblock.regenerate(right));
		}

		std::string to_string_bytecode() const
		{
			std::string res;

			res += "add ";
			res += result.to_string_bytecode();
			res += " ";
			res += left.to_string_bytecode();
			res += " ";
			res += right.to_string_bytecode();

			return res;
		}
	};

	struct Instruction_CMP
	{
		VarIDType left;
		VarIDType right;

		_debug_(std::string _debug_info;);

		void generate_bytecode(const FunctionBlock &funcblock, std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::CMP);
			pack_number(bytecode, funcblock.regenerate(left));
			pack_number(bytecode, funcblock.regenerate(right));
		}

		std::string to_string_bytecode() const
		{
			std::string res;

			res += "cmp ";
			res += left.to_string_bytecode();
			res += " ";
			res += right.to_string_bytecode();

			return res;
		}
	};

	struct Instruction_JMP
	{
		CodePosMark mark;

		_debug_(std::string _debug_info;);

		void generate_bytecode(const FunctionBlock &funcblock, std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::JMP);
			pack_number(bytecode, funcblock.regenerate(mark));
		}

		std::string to_string_bytecode() const
		{
			std::string res;

			res += "jmp ";
			res += "mark" + util::num_to_str(mark.id);

			return res;
		}
	};

	struct Instruction_JE
	{
		CodePosMark mark;

		_debug_(std::string _debug_info;);

		void generate_bytecode(const FunctionBlock &funcblock, std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::JE);
			pack_number(bytecode, funcblock.regenerate(mark));
		}

		std::string to_string_bytecode() const
		{
			std::string res;

			res += "je ";
			res += "mark" + util::num_to_str(mark.id);

			return res;
		}
	};

	struct Instruction_JNE
	{
		CodePosMark mark;

		_debug_(std::string _debug_info;);

		void generate_bytecode(const FunctionBlock &funcblock, std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::JNE);
			pack_number(bytecode, funcblock.regenerate(mark));
		}

		std::string to_string_bytecode() const
		{
			std::string res;

			res += "jne ";
			res += "mark" + util::num_to_str(mark.id);

			return res;
		}
	};

	struct Instruction_JGT
	{
		CodePosMark mark;

		_debug_(std::string _debug_info;);

		void generate_bytecode(const FunctionBlock &funcblock, std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::JGT);
			pack_number(bytecode, funcblock.regenerate(mark));
		}

		std::string to_string_bytecode() const
		{
			std::string res;

			res += "jgt ";
			res += "mark" + util::num_to_str(mark.id);

			return res;
		}
	};

	struct Instruction_JLE
	{
		CodePosMark mark;

		_debug_(std::string _debug_info;);

		void generate_bytecode(const FunctionBlock &funcblock, std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::JLE);
			pack_number(bytecode, funcblock.regenerate(mark));
		}

		std::string to_string_bytecode() const
		{
			std::string res;

			res += "jle ";
			res += "mark" + util::num_to_str(mark.id);

			return res;
		}
	};

	struct Instruction_JLT
	{
		CodePosMark mark;

		_debug_(std::string _debug_info;);

		void generate_bytecode(const FunctionBlock &funcblock, std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::JLT);
			pack_number(bytecode, funcblock.regenerate(mark));
		}

		std::string to_string_bytecode() const
		{
			std::string res;

			res += "jlt ";
			res += "mark" + util::num_to_str(mark.id);

			return res;
		}
	};

	struct Instruction_JGE
	{
		CodePosMark mark;

		_debug_(std::string _debug_info;);

		void generate_bytecode(const FunctionBlock &funcblock, std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::JGE);
			pack_number(bytecode, funcblock.regenerate(mark));
		}

		std::string to_string_bytecode() const
		{
			std::string res;

			res += "jge ";
			res += "mark" + util::num_to_str(mark.id);

			return res;
		}
	};

	struct Instruction_HLT
	{
		_debug_(std::string _debug_info;);

		void generate_bytecode(const FunctionBlock &funcblock, std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::HLT);
		}

		std::string to_string_bytecode() const
		{
			std::string res;

			res += "hlt";

			return res;
		}
	};

	using InstructionType_Base = std::variant<
		Instruction_NOP,

		Instruction_RET,
		Instruction_CALL,
		Instruction_MOV,
		Instruction_DGET,
		Instruction_DSET,
		Instruction_LC,
		Instruction_ADD,

		Instruction_CMP,

		Instruction_JMP,
		Instruction_JE,   // Zero,
		Instruction_JNE,  // NotZero,
		Instruction_JGT,  // Positive,
		Instruction_JLE,  // NotPositive,
		Instruction_JLT,  // Negative,
		Instruction_JGE,  // NotNegative,

		Instruction_HLT
	>;

	struct InstructionType : InstructionType_Base
	{
		template <typename T>
		InstructionType(const T &v) : InstructionType_Base(v)
		{
		}

		template <typename T>
		InstructionType(T &&v) : InstructionType_Base(std::move(v))
		{
		}

		void generate_bytecode(const FunctionBlock &funcblock, std::vector<uint8_t> &bytecode) const
		{
			const auto &visitor = [&](const auto &instr) {
				return instr.generate_bytecode(funcblock, bytecode);
			};

			return std::visit(visitor, *this);
		}

		std::string to_string_bytecode() const
		{
			const auto &visitor = [&](const auto &instr) {
				return instr.to_string_bytecode();
			};

			return std::visit(visitor, *this);
		}

		_debug_(
			std::string _get_debug_info() const
			{
				const auto &visitor = [&](const auto &instr) {
					return instr._debug_info;
				};

				return std::visit(visitor, *this);
			}
		);
	};

	struct FunctionScope
	{
		std::unordered_map<std::string, LocalVariableID> variables;
	};

	struct InstructionBlock
	{
		std::vector<InstructionType> instructions;
		size_t code_offset = 0;

		_debug_(std::string _debug_info;);
	};

	enum class EToResolveMarkType
	{
		Break,
		Continue,
	};

	struct ToResolveMark
	{
		EToResolveMarkType type;
		size_t instruction_block_id;
		size_t instruction_id;
		size_t value_instruction_id;
	};

	struct FunctionBlock
	{
		std::vector<InstructionBlock> instruction_blocks;
		size_t code_offset = 0;

		size_t scopes_variables_size = 0;
		size_t captures_size = 0;
		size_t parameters_size = 0;

		std::unordered_map<std::string, CaptureVariableID> captures;
		std::unordered_map<std::string, ParameterVariableID> parameters;

		std::vector<FunctionScope> scopes;

		std::vector<ToResolveMark> to_resolve_marks;

		_debug_(std::string _debug_func_name;);
		_debug_(std::unordered_map<size_t _comma_ FunctionBlock *> _debug_lc;);

		VarIDType regenerate(VarIDType id) const
		{
			const auto &visitor = [&](const auto &v) -> VarIDType {
				using T = std::decay_t<decltype(v)>;

				if constexpr (std::is_same_v<T, LocalVariableID>)
				{
					return v;
				}
				else
				if constexpr (std::is_same_v<T, StaticVariableID>)
				{
					return v;
				}
				else
				if constexpr (std::is_same_v<T, CaptureVariableID>)
				{
					return LocalVariableID{ scopes_variables_size + captures_size - v.id - 1 };
				}
				else
				if constexpr (std::is_same_v<T, ParameterVariableID>)
				{
					return LocalVariableID{ scopes_variables_size + captures_size + parameters_size - v.id - 1 };
				}
				else
				if constexpr (std::is_same_v<T, ConditionType>)
				{
					throw 1;
				}
				else
				if constexpr (std::is_same_v<T, CodePosMark>)
				{
					return LocalVariableID{ code_offset + instruction_blocks[v.id].code_offset };
				}
				else
				{
					static_assert(vm::always_false_v<T>, "Unknown type");
				}
			};

			return std::visit(visitor, id);
		}
	};

	std::vector<StaticType> statics;
	std::vector<std::unique_ptr<FunctionBlock>> function_blocks;
	std::vector<FunctionBlock *> active_function_blocks;

	void push_function_block()
	{
		auto &function_block = active_function_blocks.emplace_back(function_blocks.emplace_back(std::make_unique<FunctionBlock>()).get());
		function_block->instruction_blocks.emplace_back();
	}

	void pop_function_block()
	{
		assert(active_function_blocks.size() > 0);
		active_function_blocks.pop_back();
	}

	void push_function_scope()
	{
		active_function_blocks.back()->scopes.emplace_back();
	}

	void pop_function_scope()
	{
		assert(active_function_blocks.size() > 0);
		active_function_blocks.back()->scopes.pop_back();
	}

	[[nodiscard]]
	StaticVariableID add_static_str(const std::string val)
	{
		statics.push_back(val);
		return { statics.size() - 1 };
	}

	[[nodiscard]]
	StaticVariableID add_static_i64(int64_t val)
	{
		statics.push_back(val);
		return { statics.size() - 1 };
	}

	[[nodiscard]]
	StaticVariableID add_static_lambda(LambdaInfo val)
	{
		statics.push_back(val);
		return { statics.size() - 1 };
	}

	[[nodiscard]]
	StaticVariableID add_static_null()
	{
		statics.push_back(StaticNull{});
		return { statics.size() - 1 };
	}

	[[nodiscard]]
	LocalVariableID add_local_variable()
	{
		assert(active_function_blocks.size() > 0);
		return { active_function_blocks.back()->scopes_variables_size++ };
	}

	[[nodiscard]]
	ParameterVariableID add_local_parameter()
	{
		assert(active_function_blocks.size() > 0);
		return { active_function_blocks.back()->parameters_size++ };
	}

	[[nodiscard]]
	CaptureVariableID add_local_capture()
	{
		assert(active_function_blocks.size() > 0);
		return { active_function_blocks.back()->captures_size++ };
	}

	void add_instruction(size_t instruction_block_id, InstructionType instruction)
	{
		active_function_blocks.back()->instruction_blocks[instruction_block_id].instructions.push_back(instruction);
	}

	void add_instruction(InstructionType instruction)
	{
		add_instruction(get_instruction_block_id(), instruction);
	}

	size_t get_instruction_block_id() const
	{
		return active_function_blocks.back()->instruction_blocks.size() - 1;
	}

	void add_instruction_block()
	{
		active_function_blocks.back()->instruction_blocks.emplace_back();
	}

	void declare_local_variable(const std::string &token, LocalVariableID id)
	{
		[[maybe_unused]]
		auto result = active_function_blocks.back()->scopes.back().variables.insert({ token, id });

		assert(result.second);
	}

	void declare_local_parameter(const std::string &token, ParameterVariableID id)
	{
		[[maybe_unused]]
		auto result = active_function_blocks.back()->parameters.insert({ token, id });

		assert(result.second);
	}

	void declare_local_capture(const std::string &token, CaptureVariableID id)
	{
		[[maybe_unused]]
		auto result = active_function_blocks.back()->captures.insert({ token, id });

		assert(result.second);
	}

	VarIDType find_local_or_dget(const std::string &tok, const mylang::$Parsed &token)
	{
		if (auto v_value = find_local(tok))
		{
			return v_value.value();
		}
		else
		{
			auto v_path = add_static_str(tok);
			auto v_result_dget = add_local_variable();

			add_instruction(Instruction_DGET {
				.result = v_result_dget,
				.dict = StaticVariableID{ (size_t)StaticGlobalType::ResolveDict },
				.path = v_path,
				_debug_(._debug_info = util::minimize_whitespaces(mylang::helpers::ansii_colored(token, ansii_colors, default_ansii_color)))
			});

			return v_result_dget;
		}
	}

	std::optional<VarIDType> find_local(const std::string &tok)
	{
		const auto &active_function = active_function_blocks.back();

		{
			const auto &scopes = active_function->scopes;

			for (auto it_scope = scopes.rbegin(); it_scope != scopes.rend(); ++it_scope)
			{
				const auto &locals = it_scope->variables;

				if (auto it_local_result = locals.find(tok); it_local_result != locals.end())
				{
					return { it_local_result->second };
				}
			}
		}

		{
			const auto &locals = active_function->parameters;

			if (auto it_local_result = locals.find(tok); it_local_result != locals.end())
			{
				return { it_local_result->second };
			}
		}

		{
			const auto &locals = active_function->captures;

			if (auto it_local_result = locals.find(tok); it_local_result != locals.end())
			{
				return { it_local_result->second };
			}
		}

		return std::nullopt; // resolve in runtime
	}


	void collect_captures(const mylang::$Parsed &par, std::vector<std::string> &capture_tokens, std::vector<std::string> &ignore_tokens)
	{
		if (par.identifier == mylang::$IdentifierType::$i_expr_function)
		{
			size_t ignore_tokens_save_size = ignore_tokens.size();

			const auto &expr_function = par;
			const auto &expr_function_$g0 = expr_function.get(2, mylang::$IdentifierType::$i_expr_function_$g0);
			const auto &expr_block = expr_function.get(5, mylang::$IdentifierType::$i_expr_block);

			if (expr_function_$g0.size() > 0)
			{
				const auto &expr_function_$g0_$g0 = expr_function_$g0.get(0, mylang::$IdentifierType::$i_expr_function_$g0_$g0);
				const auto &token = expr_function_$g0_$g0.get(0, mylang::$IdentifierType::$i_token);
				const auto &expr_function_$g0_$g0_$g0 = expr_function_$g0_$g0.get(2, mylang::$IdentifierType::$i_expr_function_$g0_$g0_$g0);

				ignore_tokens.push_back(token.flatten());

				for (size_t i = 0; i < expr_function_$g0_$g0_$g0.size(); ++i)
				{
					const auto &expr_function_$g0_$g0_$g0_$g0 = expr_function_$g0_$g0_$g0.get(i, mylang::$IdentifierType::$i_expr_function_$g0_$g0_$g0_$g0);
					const auto &token = expr_function_$g0_$g0_$g0_$g0.get(2, mylang::$IdentifierType::$i_token);

					ignore_tokens.push_back(token.flatten());
				}
			}

			for (const auto &g : expr_block.group)
				collect_captures(g, capture_tokens, ignore_tokens);

			ignore_tokens.resize(ignore_tokens_save_size);
		}
		else
		if (par.identifier == mylang::$IdentifierType::$i_token_path)
		{
			const auto &token_path = par;
			const auto &token = token_path.get(0, mylang::$IdentifierType::$i_token);
			std::string tok = token.flatten();

			if (std::find(ignore_tokens.begin(), ignore_tokens.end(), tok) == ignore_tokens.end())
			{
				capture_tokens.push_back(tok);
				ignore_tokens.push_back(tok);
			}
		}
		else
		{
			for (const auto &g : par.group)
				collect_captures(g, capture_tokens, ignore_tokens);
		}
	}


	[[nodiscard]]
	VarIDType compile_expr(const mylang::$Parsed &par)
	{
		switch (par.identifier)
		{
			case mylang::$IdentifierType::$i_expr:
				{
					const auto &expr = par;
					return compile_expr(expr);
				}
			case mylang::$IdentifierType::$i_expr_compare:
				{
					const auto &expr_compare = par;

					const auto &expr_left = expr_compare.get(0);
					const auto &expr_compare_$g0 = expr_compare.get(1, mylang::$IdentifierType::$i_expr_compare_$g0);

					auto v_expr_left = compile_expr(expr_left);

					if (expr_compare_$g0.size() > 0)
					{
						_debug_(std::string _debug_info = util::minimize_whitespaces(mylang::helpers::ansii_colored(expr_left, ansii_colors, default_ansii_color)););

						{
							const auto &expr_compare_$g0_$g0 = expr_compare_$g0.get(0, mylang::$IdentifierType::$i_expr_compare_$g0_$g0);
							const auto &expr_compare_$g0_$g0_$g0 = expr_compare_$g0_$g0.get(1, mylang::$IdentifierType::$i_expr_compare_$g0_$g0_$g0);
							const auto &expr_right = expr_compare_$g0_$g0.get(3);

							_debug_(_debug_info += util::minimize_whitespaces(mylang::helpers::ansii_colored(expr_compare_$g0_$g0, ansii_colors, default_ansii_color)););

							auto v_expr_right = compile_expr(expr_right);

							add_instruction(Instruction_CMP {
								.left = v_expr_left,
								.right = v_expr_right,
								_debug_(._debug_info = _debug_info)
							});

							if (expr_compare_$g0_$g0_$g0.get(0).literal == "==")
							{
								v_expr_left = ConditionType::Zero;
							}
							else
							if (expr_compare_$g0_$g0_$g0.get(0).literal == "!=")
							{
								v_expr_left = ConditionType::NotZero;
							}
							else
							if (expr_compare_$g0_$g0_$g0.get(0).literal == ">")
							{
								v_expr_left = ConditionType::Positive;
							}
							else
							if (expr_compare_$g0_$g0_$g0.get(0).literal == "<=")
							{
								v_expr_left = ConditionType::NotPositive;
							}
							else
							if (expr_compare_$g0_$g0_$g0.get(0).literal == "<")
							{
								v_expr_left = ConditionType::Negative;
							}
							else
							if (expr_compare_$g0_$g0_$g0.get(0).literal == ">=")
							{
								v_expr_left = ConditionType::NotNegative;
							}
							else
							{
								throw 1;
							}
						}
					}

					return v_expr_left;
				}
			case mylang::$IdentifierType::$i_expr_add:
				{
					const auto &expr_add = par;

					const auto &expr_left = expr_add.get(0);
					const auto &expr_add_$g0 = expr_add.get(1, mylang::$IdentifierType::$i_expr_add_$g0);

					auto v_expr_left = compile_expr(expr_left);

					if (expr_add_$g0.size() > 0)
					{
						_debug_(std::string _debug_info = util::minimize_whitespaces(mylang::helpers::ansii_colored(expr_left, ansii_colors, default_ansii_color)););

						for (size_t i = 0; i < expr_add_$g0.size(); ++i)
						{
							const auto &expr_add_$g0_$g0 = expr_add_$g0.get(i, mylang::$IdentifierType::$i_expr_add_$g0_$g0);
							const auto &expr_add_$g0_$g0_$g0 = expr_add_$g0_$g0.get(1, mylang::$IdentifierType::$i_expr_add_$g0_$g0_$g0);
							const auto &expr_right = expr_add_$g0_$g0.get(3);

							_debug_(_debug_info += util::minimize_whitespaces(mylang::helpers::ansii_colored(expr_add_$g0_$g0, ansii_colors, default_ansii_color)););

							auto v_expr_right = compile_expr(expr_right);

							if (expr_add_$g0_$g0_$g0.get(0).literal == "+")
							{
								auto v_result = add_local_variable();

								add_instruction(Instruction_ADD {
									.result = v_result,
									.left = v_expr_left,
									.right = v_expr_right,
									_debug_(._debug_info = _debug_info)
								});

								v_expr_left = v_result;
							}
							else
							{
								throw 1;
							}
						}
					}

					return v_expr_left;
				}
			case mylang::$IdentifierType::$i_expr_mul:
				{
					const auto &expr_mul = par;

					const auto &expr_left = expr_mul.get(0);

					auto v_expr_left = compile_expr(expr_left);

					// TODO;

					return v_expr_left;
				}
			case mylang::$IdentifierType::$i_expr_primitive:
				{
					const auto &expr_primitive = par;

					const auto &expr = expr_primitive.get(0);

					return compile_expr(expr);
				}
			case mylang::$IdentifierType::$i_number:
				{
					const auto &number = par;

					return add_static_i64(stoll(number.flatten()));
				}
			case mylang::$IdentifierType::$i_expr_assign:
				{
					const auto &expr_assign = par;
					const auto &token_path = expr_assign.get(0, mylang::$IdentifierType::$i_token_path);
					const auto &expr = expr_assign.get(4);

					auto v_expr = compile_expr(expr);

					// from token_path parsing
					{
						const auto &token = token_path.get(0, mylang::$IdentifierType::$i_token);
						const auto &token_path_$g0 = token_path.get(1, mylang::$IdentifierType::$i_token_path_$g0);

						std::string tok = token.flatten();

						VarIDType v_result = find_local_or_dget(tok, token);

						if (token_path_$g0.size() > 0)
						{
							for (size_t i = 0; i < token_path_$g0.size(); ++i)
							{
								const auto &token_path_$g0_$g0 = token_path_$g0.get(i, mylang::$IdentifierType::$i_token_path_$g0_$g0);
								const auto &token = token_path_$g0_$g0.get(3, mylang::$IdentifierType::$i_token);

								std::string tok = token.flatten();

								auto v_path = add_static_str(tok);

								if (i + 1 < token_path_$g0.size())
								{
									auto v_new_result = add_local_variable();

									add_instruction(Instruction_DGET {
										.result = v_new_result,
										.dict = v_result,
										.path = v_path,
										_debug_(._debug_info = util::minimize_whitespaces(mylang::helpers::ansii_colored(token, ansii_colors, default_ansii_color)))
									});

									v_result = v_new_result;
								}
								else
								{
									add_instruction(Instruction_DSET {
										.dict = v_result,
										.path = v_path,
										.val = v_expr,
										_debug_(._debug_info = util::minimize_whitespaces(mylang::helpers::ansii_colored(token, ansii_colors, default_ansii_color)))
									});

									v_result = v_expr;
								}
							}
						}

						add_instruction(Instruction_MOV {
							.dest = v_result,
							.src = v_expr,
							_debug_(._debug_info = util::minimize_whitespaces(mylang::helpers::ansii_colored(expr_assign, ansii_colors, default_ansii_color)))
						});

						return v_result;
					}
				}
			case mylang::$IdentifierType::$i_token_path:
				{
					const auto &token_path = par;
					const auto &token = token_path.get(0, mylang::$IdentifierType::$i_token);
					const auto &token_path_$g0 = token_path.get(1, mylang::$IdentifierType::$i_token_path_$g0);

					std::string tok = token.flatten();

					VarIDType v_result = find_local_or_dget(tok, token);

					if (token_path_$g0.size() > 0)
					{
						for (size_t i = 0; i < token_path_$g0.size(); ++i)
						{
							const auto &token_path_$g0_$g0 = token_path_$g0.get(i, mylang::$IdentifierType::$i_token_path_$g0_$g0);
							const auto &token = token_path_$g0_$g0.get(3, mylang::$IdentifierType::$i_token);

							std::string tok = token.flatten();

							auto v_path = add_static_str(tok);
							auto v_new_result = add_local_variable();

							add_instruction(Instruction_DGET {
								.result = v_new_result,
								.dict = v_result,
								.path = v_path,
								_debug_(._debug_info = util::minimize_whitespaces(mylang::helpers::ansii_colored(token, ansii_colors, default_ansii_color)))
							});

							v_result = v_new_result;
						}
					}

					return v_result;
				}
			case mylang::$IdentifierType::$i_expr_call:
				{
					const auto &expr_call = par;
					const auto &expr_call_$g0 = expr_call.get(0, mylang::$IdentifierType::$i_expr_call_$g0);
					const auto &func = expr_call_$g0.get(0);
					const auto &args_maybe = expr_call.get(4, mylang::$IdentifierType::$i_expr_call_$g1);

					auto v_func = compile_expr(func);

					std::vector<VarIDType> v_args;

					if (args_maybe.size() > 0)
					{
						const auto &args = args_maybe.get(0, mylang::$IdentifierType::$i_expr_call_$g1_$g0);
						const auto &expr = args.get(0);
						const auto &more_args_maybe = args.get(2, mylang::$IdentifierType::$i_expr_call_$g1_$g0_$g0);

						v_args.push_back(compile_expr(expr));

						if (more_args_maybe.size() > 0)
						{
							for (size_t i = 0; i < more_args_maybe.size(); ++i)
							{
								const auto &more_args = more_args_maybe.get(i, mylang::$IdentifierType::$i_expr_call_$g1_$g0_$g0_$g0);
								const auto &expr = more_args.get(2);
								v_args.push_back(compile_expr(expr));
							}
						}
					}

					add_instruction(Instruction_CALL {
						.func = v_func,
						.args = std::move(v_args),
						_debug_(._debug_info = util::minimize_whitespaces(mylang::helpers::ansii_colored(expr_call, ansii_colors, default_ansii_color)))
					});

					auto v_result = add_local_variable();

					add_instruction(Instruction_MOV {
						.dest = v_result,
						.src = StaticVariableID{ (size_t)StaticGlobalType::RetVal },
						_debug_(._debug_info = "<retval> = " + util::minimize_whitespaces(mylang::helpers::ansii_colored(expr_call, ansii_colors, default_ansii_color)))
					});

					return v_result;
				}
			case mylang::$IdentifierType::$i_expr_function:
				{
					const auto &expr_function = par;
					const auto &expr_function_$g0 = expr_function.get(2, mylang::$IdentifierType::$i_expr_function_$g0);
					const auto &expr_block = expr_function.get(5, mylang::$IdentifierType::$i_expr_block);

					std::vector<VarIDType> captures;
					std::vector<std::string> capture_tokens;
					std::vector<std::string> ignore_tokens;
					collect_captures(expr_function, capture_tokens, ignore_tokens);

					for (const auto &capture_token : capture_tokens)
						captures.push_back(find_local_or_dget(capture_token, expr_function));

					auto result_lambda = add_static_lambda(LambdaInfo {
						// .function_block = stacks.back().function_block,
					});

					VarIDType result = result_lambda;

					if (capture_tokens.size() > 0)
					{
						auto new_result = add_local_variable();

						add_instruction(Instruction_LC {
							.result = new_result,
							.func = result,
							.captures = captures,
							_debug_(._debug_info = util::minimize_whitespaces(mylang::helpers::ansii_colored(expr_function, ansii_colors, default_ansii_color)))
						});

						result = new_result;
					}

					push_function_block();
					push_function_scope();

					ExitScope _onexit([&]() {
						pop_function_scope();
						pop_function_block();
					});

					std::get<LambdaInfo>(statics[result_lambda.id]).function_block = active_function_blocks.back();

					_debug_({
						if (capture_tokens.size() > 0)
							active_function_blocks[active_function_blocks.size() - 2]->_debug_lc[std::get_if<LocalVariableID>(&result)->id] = active_function_blocks.back();
					});

					std::vector<std::string> args_tokens;

					if (expr_function_$g0.size() > 0)
					{
						const auto &expr_function_$g0_$g0 = expr_function_$g0.get(0, mylang::$IdentifierType::$i_expr_function_$g0_$g0);
						const auto &token = expr_function_$g0_$g0.get(0, mylang::$IdentifierType::$i_token);
						const auto &expr_function_$g0_$g0_$g0 = expr_function_$g0_$g0.get(2, mylang::$IdentifierType::$i_expr_function_$g0_$g0_$g0);

						args_tokens.push_back(token.flatten());

						for (size_t i = 0; i < expr_function_$g0_$g0_$g0.size(); ++i)
						{
							const auto &expr_function_$g0_$g0_$g0_$g0 = expr_function_$g0_$g0_$g0.get(i, mylang::$IdentifierType::$i_expr_function_$g0_$g0_$g0_$g0);
							const auto &token = expr_function_$g0_$g0_$g0_$g0.get(2, mylang::$IdentifierType::$i_token);

							args_tokens.push_back(token.flatten());
						}
					}

					for (const auto &t : capture_tokens)
						declare_local_capture(t, add_local_capture());

					for (const auto &t : args_tokens)
						declare_local_parameter(t, add_local_parameter());

					auto v_ret = compile_expr_block(expr_block);

					if (!v_ret.is_null())
					{
						add_instruction(Instruction_MOV {
							.dest = StaticVariableID{ (size_t)StaticGlobalType::RetVal },
							.src = v_ret,
							_debug_(._debug_info = "// Return value")
						});
					}

					add_instruction(Instruction_RET {
						_debug_(._debug_info = "// Return from function")
					});

					return result;
				}
			case mylang::$IdentifierType::$i_expr_block:
				{
					const auto &expr_block = par;
					return compile_expr_block(expr_block);
				}
			case mylang::$IdentifierType::$i_expr_if:
				{
					const auto &expr_if = par;
					const auto &expr_condition = expr_if.get(4);
					const auto &expr_block_if = expr_if.get(8, mylang::$IdentifierType::$i_expr_block);
					const auto &expr_block_else_maybe = expr_if.get(9, mylang::$IdentifierType::$i_expr_if_$g0);

					auto v_condition = compile_expr(expr_condition);
					auto condition = std::get_if<ConditionType>(&v_condition);

					if (!condition)
						throw 1;

					auto if_pos = get_instruction_block_id();

					add_instruction_block();
					_debug_(active_function_blocks.back()->instruction_blocks.back()._debug_info = "if (" + util::minimize_whitespaces(mylang::helpers::ansii_colored(expr_condition, ansii_colors, default_ansii_color)) + ")");
					auto v_if_block = compile_expr(expr_block_if);
					auto if_pos_before = get_instruction_block_id();
					add_instruction_block();
					auto if_pos_after = get_instruction_block_id();

					VarIDType v_result = StaticVariableID{ (size_t)StaticGlobalType::Null };

					if (!v_if_block.is_null())
					{
						v_result = add_local_variable();

						add_instruction(if_pos_before, Instruction_MOV {
							.dest = v_result,
							.src = v_if_block,
						});
					}

					// inverse logic: if-not then jump
					switch (*condition)
					{
						case ConditionType::Zero:
							add_instruction(if_pos, Instruction_JNE {
								.mark = CodePosMark{ .id = if_pos_after },
							});
							break;
						case ConditionType::NotZero:
							add_instruction(if_pos, Instruction_JE {
								.mark = CodePosMark{ .id = if_pos_after },
							});
							break;
						case ConditionType::Positive:
							add_instruction(if_pos, Instruction_JLE {
								.mark = CodePosMark{ .id = if_pos_after },
							});
							break;
						case ConditionType::NotPositive:
							add_instruction(if_pos, Instruction_JGT {
								.mark = CodePosMark{ .id = if_pos_after },
							});
							break;
						case ConditionType::Negative:
							add_instruction(if_pos, Instruction_JGE {
								.mark = CodePosMark{ .id = if_pos_after },
							});
							break;
						case ConditionType::NotNegative:
							add_instruction(if_pos, Instruction_JLT {
								.mark = CodePosMark{ .id = if_pos_after },
							});
							break;
					}

					if (expr_block_else_maybe.size() > 0)
					{
						const auto &expr_block_else_g = expr_block_else_maybe.get(0, mylang::$IdentifierType::$i_expr_if_$g0_$g0);
						const auto &expr_block_else = expr_block_else_g.get(3, mylang::$IdentifierType::$i_expr_block);

						auto else_pos_before = get_instruction_block_id();
						auto v_else_block = compile_expr(expr_block_else);
						_debug_(active_function_blocks.back()->instruction_blocks.back()._debug_info = "else <- if (" + util::minimize_whitespaces(mylang::helpers::ansii_colored(expr_condition, ansii_colors, default_ansii_color)) + ")");
						add_instruction_block();
						auto else_pos_after = get_instruction_block_id();

						if (v_if_block.is_null() != v_else_block.is_null())
							throw 1;

						if (!v_else_block.is_null())
						{
							add_instruction(else_pos_before, Instruction_MOV {
								.dest = v_result,
								.src = v_else_block,
							});
						}

						add_instruction(if_pos_before, Instruction_JMP {
							.mark = CodePosMark{ .id = else_pos_after },
						});
					}

					return v_result;
				}
			case mylang::$IdentifierType::$i_expr_while:
				{
					const auto &expr_while = par;
					const auto &expr_condition = expr_while.get(4);
					const auto &expr_block_while = expr_while.get(8, mylang::$IdentifierType::$i_expr_block);
					const auto &expr_block_else_maybe = expr_while.get(9, mylang::$IdentifierType::$i_expr_while_$g0);

					std::vector<ToResolveMark> to_resolve_marks;

					add_instruction_block();

					auto v_condition = compile_expr(expr_condition);
					auto condition = std::get_if<ConditionType>(&v_condition);

					if (!condition)
						throw 1;

					auto while_pos = get_instruction_block_id();

					add_instruction_block();
					_debug_(active_function_blocks.back()->instruction_blocks.back()._debug_info = "while (" + util::minimize_whitespaces(mylang::helpers::ansii_colored(expr_condition, ansii_colors, default_ansii_color)) + ")");
					auto v_while_block = compile_expr(expr_block_while);

					if (!v_while_block.is_null())
						throw 1;

					{
						auto &fb = active_function_blocks.back();
						auto it = std::remove_if(fb->to_resolve_marks.begin(), fb->to_resolve_marks.end(), [](const auto &v) {
							return v.type == EToResolveMarkType::Break || v.type == EToResolveMarkType::Continue;
						});
						std::move(it, fb->to_resolve_marks.end(), std::back_insert_iterator(to_resolve_marks));
						fb->to_resolve_marks.erase(it, fb->to_resolve_marks.end());
					}

					add_instruction(Instruction_JMP {
						.mark = CodePosMark{ .id = while_pos },
					});

					auto while_pos_before = get_instruction_block_id();
					add_instruction_block();
					auto while_pos_after = get_instruction_block_id();
					auto while_pos_last = get_instruction_block_id();

					VarIDType v_result = StaticVariableID{ (size_t)StaticGlobalType::Null };

					// inverse logic: if-not then jump
					switch (*condition)
					{
						case ConditionType::Zero:
							add_instruction(while_pos, Instruction_JNE {
								.mark = CodePosMark{ .id = while_pos_after },
							});
							break;
						case ConditionType::NotZero:
							add_instruction(while_pos, Instruction_JE {
								.mark = CodePosMark{ .id = while_pos_after },
							});
							break;
						case ConditionType::Positive:
							add_instruction(while_pos, Instruction_JLE {
								.mark = CodePosMark{ .id = while_pos_after },
							});
							break;
						case ConditionType::NotPositive:
							add_instruction(while_pos, Instruction_JGT {
								.mark = CodePosMark{ .id = while_pos_after },
							});
							break;
						case ConditionType::Negative:
							add_instruction(while_pos, Instruction_JGE {
								.mark = CodePosMark{ .id = while_pos_after },
							});
							break;
						case ConditionType::NotNegative:
							add_instruction(while_pos, Instruction_JLT {
								.mark = CodePosMark{ .id = while_pos_after },
							});
							break;
					}

					if (expr_block_else_maybe.size() > 0)
					{
						const auto &expr_block_else_g = expr_block_else_maybe.get(0, mylang::$IdentifierType::$i_expr_while_$g0_$g0);
						const auto &expr_block_else = expr_block_else_g.get(3, mylang::$IdentifierType::$i_expr_block);

						auto else_pos_before = get_instruction_block_id();
						auto v_else_block = compile_expr(expr_block_else);
						_debug_(active_function_blocks.back()->instruction_blocks.back()._debug_info = "else <- if (" + util::minimize_whitespaces(mylang::helpers::ansii_colored(expr_condition, ansii_colors, default_ansii_color)) + ")");
						add_instruction_block();
						auto else_pos_after = get_instruction_block_id();
						while_pos_last = get_instruction_block_id();

						if (!v_else_block.is_null())
						{
							if (v_result.is_null())
								v_result = add_local_variable();

							add_instruction(else_pos_before, Instruction_MOV {
								.dest = v_result,
								.src = v_else_block,
							});
						}
					}

					for (const auto &to_resolve_mark : to_resolve_marks)
					{
						switch (to_resolve_mark.type)
						{
							case EToResolveMarkType::Break:
								{
									auto jmp = std::get_if<Instruction_JMP>(&active_function_blocks.back()->instruction_blocks[to_resolve_mark.instruction_block_id].instructions[to_resolve_mark.instruction_id]);

									if (!jmp)
										throw 1;

									jmp->mark = CodePosMark{ .id = while_pos_last };

									if (to_resolve_mark.value_instruction_id != (size_t)-1)
									{
										if (v_result.is_null())
											v_result = add_local_variable();

										if (auto mov = std::get_if<Instruction_MOV>(&active_function_blocks.back()->instruction_blocks[to_resolve_mark.instruction_block_id].instructions[to_resolve_mark.value_instruction_id]))
											mov->dest = v_result;
									}

									break;
								}
							case EToResolveMarkType::Continue:
								{
									auto jmp = std::get_if<Instruction_JMP>(&active_function_blocks.back()->instruction_blocks[to_resolve_mark.instruction_block_id].instructions[to_resolve_mark.instruction_id]);

									if (!jmp)
										throw 1;

									jmp->mark = CodePosMark{ .id = while_pos };

									break;
								}
						}
					}

					return v_result;
				}
		}

		throw 1;
	}

	VarIDType compile_expr_block(const mylang::$Parsed &expr_block)
	{
		assert(expr_block.identifier == mylang::$IdentifierType::$i_expr_block);

		const auto &expr_block_$g0 = expr_block.get(2, mylang::$IdentifierType::$i_expr_block_$g0);
		const auto &expr_block_$g1 = expr_block.get(3, mylang::$IdentifierType::$i_expr_block_$g1);

		push_function_scope();

		ExitScope _onexit([&]() {
			pop_function_scope();
		});

		for (size_t i = 0; i < expr_block_$g0.size(); ++i)
		{
			const auto &expr_block_$g0_$g0 = expr_block_$g0.get(i, mylang::$IdentifierType::$i_expr_block_$g0_$g0);
			const auto &statement = expr_block_$g0_$g0.get(0);
			compile_statement(statement);
		}

		if (expr_block_$g1.size() > 0)
		{
			const auto &expr_block_$g1_$g0 = expr_block_$g1.get(0, mylang::$IdentifierType::$i_expr_block_$g1_$g0);
			const auto &expr = expr_block_$g1_$g0.get(0);
			return compile_expr(expr);
		}

		return StaticVariableID{ (size_t)StaticGlobalType::Null };
	}

	void compile_statement_if(const mylang::$Parsed &statement_if)
	{
		assert(statement_if.identifier == mylang::$IdentifierType::$i_statement_if);

		const auto &expr_if = statement_if.get(0, mylang::$IdentifierType::$i_expr_if);

		auto v_expr = compile_expr(expr_if);
		(void)v_expr;
	}

	void compile_statement_while(const mylang::$Parsed &statement_while)
	{
		assert(statement_while.identifier == mylang::$IdentifierType::$i_statement_while);

		const auto &expr_while = statement_while.get(0, mylang::$IdentifierType::$i_expr_while);

		auto v_expr = compile_expr(expr_while);
		(void)v_expr;
	}

	void compile_statement_let(const mylang::$Parsed &statement_let)
	{
		assert(statement_let.identifier == mylang::$IdentifierType::$i_statement_let);

		const auto &token = statement_let.get(3, mylang::$IdentifierType::$i_token);
		const auto &expr = statement_let.get(7);

		auto v_expr = compile_expr(expr);
		auto v_token = add_local_variable();

		_debug_({
			if (const auto *static_v_expr = std::get_if<StaticVariableID>(&v_expr))
			{
				if (auto *lambda_v_expr = std::get_if<LambdaInfo>(&statics[static_v_expr->id]))
				{
					lambda_v_expr->function_block->_debug_func_name = token.flatten();
				}
			}
			else
			if (const auto *local_v_expr = std::get_if<LocalVariableID>(&v_expr))
			{
				if (auto function_block_it = active_function_blocks.back()->_debug_lc.find(local_v_expr->id); function_block_it != active_function_blocks.back()->_debug_lc.end())
				{
					function_block_it->second->_debug_func_name = token.flatten();
				}
			}
		});

		declare_local_variable(token.flatten(), v_token);
		add_instruction(Instruction_MOV{
			.dest = v_token,
			.src = v_expr,
			_debug_(._debug_info = util::minimize_whitespaces(mylang::helpers::ansii_colored(statement_let, ansii_colors, default_ansii_color)))
		});
	}

	void compile_statement_return(const mylang::$Parsed &statement_return)
	{
		assert(statement_return.identifier == mylang::$IdentifierType::$i_statement_return);

		const auto &statement_return_$g0 = statement_return.get(1, mylang::$IdentifierType::$i_statement_return_$g0);

		if (statement_return_$g0.size() > 0)
		{
			const auto &statement_return_$g0_$g0 = statement_return_$g0.get(0, mylang::$IdentifierType::$i_statement_return_$g0_$g0);
			const auto &expr = statement_return_$g0_$g0.get(1);

			auto v_expr = compile_expr(expr);

			add_instruction(Instruction_MOV {
				.dest = StaticVariableID{ (size_t)StaticGlobalType::RetVal },
				.src = v_expr,
				_debug_(._debug_info = util::minimize_whitespaces(mylang::helpers::ansii_colored(expr, ansii_colors, default_ansii_color)))
			});
		}

		add_instruction(Instruction_RET {
			_debug_(._debug_info = util::minimize_whitespaces(mylang::helpers::ansii_colored(statement_return, ansii_colors, default_ansii_color)))
		});
	}

	void compile_statement_continue(const mylang::$Parsed &statement_continue)
	{
		assert(statement_continue.identifier == mylang::$IdentifierType::$i_statement_continue);

		add_instruction(Instruction_JMP {
			.mark = CodePosMark{ (size_t)-1 },
			_debug_(._debug_info = util::minimize_whitespaces(mylang::helpers::ansii_colored(statement_continue, ansii_colors, default_ansii_color)))
		});

		function_blocks.back()->to_resolve_marks.push_back(ToResolveMark {
			.type = EToResolveMarkType::Continue,
			.instruction_block_id = function_blocks.back()->instruction_blocks.size() - 1,
			.instruction_id = function_blocks.back()->instruction_blocks.back().instructions.size() - 1,
			.value_instruction_id = (size_t)-1,
		});
	}

	void compile_statement_break(const mylang::$Parsed &statement_break)
	{
		assert(statement_break.identifier == mylang::$IdentifierType::$i_statement_break);

		const auto &statement_break_$g0 = statement_break.get(1, mylang::$IdentifierType::$i_statement_break_$g0);

		size_t value_instruction_id = (size_t)-1;

		if (statement_break_$g0.size() > 0)
		{
			const auto &statement_break_$g0_$g0 = statement_break_$g0.get(0, mylang::$IdentifierType::$i_statement_break_$g0_$g0);
			const auto &expr = statement_break_$g0_$g0.get(1);

			auto v_result = compile_expr(expr);

			add_instruction(Instruction_MOV {
				.dest = StaticVariableID{ (size_t)StaticGlobalType::Null },
				.src = v_result,
				_debug_(._debug_info = util::minimize_whitespaces(mylang::helpers::ansii_colored(statement_break, ansii_colors, default_ansii_color)))
			});

			value_instruction_id = function_blocks.back()->instruction_blocks.back().instructions.size() - 1;
		}

		add_instruction(Instruction_JMP {
			.mark = CodePosMark{ (size_t)-1 },
			_debug_(._debug_info = util::minimize_whitespaces(mylang::helpers::ansii_colored(statement_break, ansii_colors, default_ansii_color)))
		});

		function_blocks.back()->to_resolve_marks.push_back(ToResolveMark {
			.type = EToResolveMarkType::Break,
			.instruction_block_id = function_blocks.back()->instruction_blocks.size() - 1,
			.instruction_id = function_blocks.back()->instruction_blocks.back().instructions.size() - 1,
			.value_instruction_id = value_instruction_id,
		});
	}

	void compile_statement_expr(const mylang::$Parsed &statement_expr)
	{
		assert(statement_expr.identifier == mylang::$IdentifierType::$i_statement_expr);

		const auto &expr = statement_expr.get(0);

		auto v_expr = compile_expr(expr);
		(void)v_expr;
	}

	void compile_statement(const mylang::$Parsed &par)
	{
		switch (par.identifier)
		{
			case mylang::$IdentifierType::$i_statement:
				{
					const auto &statement = par;
					return compile_statement(statement.group[0]);
				}
			case mylang::$IdentifierType::$i_statement_if:
				{
					const auto &statement_if = par;
					return compile_statement_if(statement_if);
				}
			case mylang::$IdentifierType::$i_statement_while:
				{
					const auto &statement_while = par;
					return compile_statement_while(statement_while);
				}
			case mylang::$IdentifierType::$i_statement_let:
				{
					const auto &statement_let = par;
					return compile_statement_let(statement_let);
				}
			case mylang::$IdentifierType::$i_statement_return:
				{
					const auto &statement_return = par;
					return compile_statement_return(statement_return);
				}
			case mylang::$IdentifierType::$i_statement_continue:
				{
					const auto &statement_continue = par;
					return compile_statement_continue(statement_continue);
				}
			case mylang::$IdentifierType::$i_statement_break:
				{
					const auto &statement_break = par;
					return compile_statement_break(statement_break);
				}
			case mylang::$IdentifierType::$i_statement_expr:
				{
					const auto &statement_expr = par;
					return compile_statement_expr(statement_expr);
				}
		}

		throw 1;
	}

	void compile_root(const mylang::$Parsed &root)
	{
		assert(root.identifier == mylang::$IdentifierType::$i_root);

		static_assert((size_t)StaticGlobalType::_size == 3, "Update statics");
		(void)add_static_null(); // RetVal
		(void)add_static_null(); // ResolveDict
		(void)add_static_null(); // Null

		push_function_block();
		push_function_scope();

		ExitScope _onexit([&]() {
			assert(active_function_blocks.size() == 1 && active_function_blocks[0]->scopes.size() == 1);

			pop_function_scope();
			pop_function_block();
		});

		_debug_(function_blocks[0]->_debug_func_name = "_start");

		const auto &root_$g0 = root.get(1, mylang::$IdentifierType::$i_root_$g0);

		for (size_t i = 0; i < root_$g0.size(); ++i)
		{
			const auto &root_$g0_$g0 = root_$g0.get(i, mylang::$IdentifierType::$i_root_$g0_$g0);
			const auto &statement = root_$g0_$g0.get(0);

			compile_statement(statement);
		}

		add_instruction(Instruction_HLT {
			_debug_(._debug_info = "// Program end")
		});
	}

	Program generate_program()
	{
		Program program;
		program.bytes = generate_bytecode();

		program.entry_point = FunctionInfo {
			.code_offset = function_blocks[0]->code_offset,
			.stack_size = function_blocks[0]->scopes_variables_size,
		};

		for (size_t i = 0; i < statics.size(); ++i)
		{
			const auto &visitor = [&](const auto &static_var) {
				using T = std::decay_t<decltype(static_var)>;

				if constexpr (std::is_same_v<T, int64_t>)
				{
					program.statics.push_back(sptr<RuntimeValueI64>::create_bounded(static_var));
				}
				else
				if constexpr (std::is_same_v<T, std::string>)
				{
					program.statics.push_back(sptr<RuntimeValueStr>::create_bounded(static_var));
				}
				else
				if constexpr (std::is_same_v<T, Compiler::LambdaInfo>)
				{
					auto lambda = sptr<RuntimeValueLambda>::create_bounded(nullptr);
					lambda->func_info.stack_size = static_var.function_block->scopes_variables_size;
					lambda->func_info.code_offset = static_var.function_block->code_offset;
					program.statics.push_back(lambda);
				}
				else
				if constexpr (std::is_same_v<T, Compiler::StaticNull>)
				{
					program.statics.push_back({});
				}
				else
				{
					static_assert(always_false_v<T>, "Unknown type");
				}
			};

			std::visit(visitor, statics[i]);
		}

		*program.resolve({ "std", "print"}) = sptr<RuntimeValueLambda>::create_bounded([](Program &program, const std::span<ivalue_t> &args) {
			for (size_t i = 0; i < args.size(); ++i)
			{
				const auto &arg = args[i];

				if (i != 0)
					std::cout << " ";

				switch (arg->type)
				{
					case ERuntimeValueType::I64:
						std::cout << arg.casted<RuntimeValueI64>()->val;
						break;
					case ERuntimeValueType::Str:
						std::cout << arg.casted<RuntimeValueStr>()->val;
						break;
					case ERuntimeValueType::Dict:
							std::cout << "dict{ size = " << arg.casted<RuntimeValueDict>()->val.size() << " }";
						break;
					case ERuntimeValueType::Lambda:
							std::cout << "<lambda>";
						break;
				}
			}

			std::cout << "\n";
		});

		*program.resolve({ "std", "i64"}) = sptr<RuntimeValueLambda>::create_bounded([](Program &program, const std::span<ivalue_t> &args) {
			auto result = sptr<RuntimeValueI64>::create_bounded();

			if (args.size() > 0 && args[0])
			{
				switch (args[0]->type)
				{
					case ERuntimeValueType::I64:
						result->val = args[0].casted<RuntimeValueI64>()->val;
						break;
					case ERuntimeValueType::Str:
						result->val = stoll(args[0].casted<RuntimeValueStr>()->val);
						break;
				}
			}

			program.set_retval(result);
		});

		*program.resolve({ "std", "str"}) = sptr<RuntimeValueLambda>::create_bounded([](Program &program, const std::span<ivalue_t> &args) {
			auto result = sptr<RuntimeValueStr>::create_bounded();

			if (args.size() > 0 && args[0])
			{
				switch (args[0]->type)
				{
					case ERuntimeValueType::I64:
						result->val = std::to_string(args[0].casted<RuntimeValueI64>()->val);
						break;
					case ERuntimeValueType::Str:
						result->val = args[0].casted<RuntimeValueStr>()->val;
						break;
				}
			}

			program.set_retval(result);
		});

		*program.resolve({ "std", "dict"}) = sptr<RuntimeValueLambda>::create_bounded([](Program &program, const std::span<ivalue_t> &args) {
			program.set_retval(sptr<RuntimeValueDict>::create_bounded());
		});

		*program.resolve({ "std", "null"}) = sptr<RuntimeValueLambda>::create_bounded([](Program &program, const std::span<ivalue_t> &args) {
			program.set_retval({});
		});

		*program.resolve({ "exports" }) = sptr<RuntimeValueDict>::create_bounded();

		return program;
	}

	std::vector<uint8_t> generate_bytecode()
	{
		std::vector<uint8_t> bytecode;
		size_t last_bytecode_size = 0;
		bool changed = true;

		// Need to generate code multiple times to calculate all mark-offsets
		while (changed)
		{
			bytecode.clear();

			for (const auto &function_block : function_blocks)
			{
				function_block->code_offset = bytecode.size();

				for (auto &instruction_block : function_block->instruction_blocks)
				{
					instruction_block.code_offset = bytecode.size() - function_block->code_offset;

					for (const auto &instruction : instruction_block.instructions)
						instruction.generate_bytecode(*function_block, bytecode);
				}
			}

			changed = last_bytecode_size != bytecode.size();
			last_bytecode_size = bytecode.size();
		}

		return bytecode;
	}

	std::string to_string_bytecode() const
	{
		std::string result;

		result += "data:\n";

		{
			std::vector<std::string> results;
			std::vector<std::string> poses;
			size_t poses_max_length = 0;

			for (size_t i = 0; i < statics.size(); ++i)
			{
				const auto &v_static = statics[i];

				const auto &visitor = [&](const auto &v) -> std::string {
					using T = std::decay_t<decltype(v)>;

					if constexpr (std::is_same_v<T, int64_t>)
					{
						return "#" + std::to_string(v);
					}
					else
					if constexpr (std::is_same_v<T, std::string>)
					{
						return '"' + util::minimize_whitespaces(v) + '"';
					}
					else
					if constexpr (std::is_same_v<T, Compiler::LambdaInfo>)
					{
						return "lambda{ " _debug_(+ v.function_block->_debug_func_name) + " }";
					}
					else
					if constexpr (std::is_same_v<T, Compiler::StaticNull>)
					{
						if (i == (size_t)StaticGlobalType::RetVal)
							return "<retval>";

						if (i == (size_t)StaticGlobalType::ResolveDict)
							return "<resolve dict>";

						if (i == (size_t)StaticGlobalType::Null)
							return "<global null>";

						return "null";
					}
					else
					{
						static_assert(vm::always_false_v<T>, "Unknown type");
					}
				};

				results.push_back(std::visit(visitor, v_static));
				poses.push_back(std::to_string(i));
				poses_max_length = poses.back().size() > poses_max_length ? poses.back().size() : poses_max_length;
			}

			for (size_t i = 0; i < results.size(); ++i)
				result +=
					"    "
					+ std::string(poses_max_length - poses[i].size(), ' ') + poses[i] + "   "
					+ results[i]
					+ "\n";
		}

		_debug_(size_t bytes_pos = 0;);

		result += "\n";
		result += "code:\n";
		for (size_t i = 0; i < function_blocks.size(); ++i)
		{
			if (i != 0)
				result += "\n";

			const auto &function_block = function_blocks[i];

			_debug_(result += "; code offset = " + util::num_to_str(function_block->code_offset) + "\n");
			_debug_(result += "; capture size = " + std::to_string(function_block->captures_size) + "\n");
			_debug_(result += "; parameter size = " + std::to_string(function_block->parameters_size) + "\n");
			_debug_(result += "; stack size = " + std::to_string(function_block->scopes_variables_size) + "\n");
			_debug_(result += function_block->_debug_func_name + ":\n");

			std::vector<std::string> results;

			_debug_(size_t max_length = 0;);
			_debug_(size_t max_length_bytes = 0;);
			_debug_(size_t max_length_bytes_pos = 0;);

			_debug_(std::vector<uint8_t> results_debug_bytes;);
			_debug_(std::vector<std::string> results_debug_bytes_s;);
			_debug_(std::vector<std::string> results_debug_bytes_pos_s;);
			_debug_(std::vector<std::string> results_debug;);

			for (size_t instruction_block_index = 0; instruction_block_index < function_block->instruction_blocks.size(); ++instruction_block_index)
			{
				const auto &instruction_block = function_block->instruction_blocks[instruction_block_index];

				results.push_back("mark" + util::num_to_str(instruction_block_index) + ":" _debug_("   ; " + instruction_block._debug_info));

				_debug_({
					results_debug.push_back("");
					results_debug_bytes_s.push_back("");
					results_debug_bytes_pos_s.push_back("");
				});

				for (const auto &instruction : instruction_block.instructions)
				{
					results.push_back(instruction.to_string_bytecode());

					_debug_({
						results_debug.push_back(instruction._get_debug_info());
						results_debug_bytes.clear();
						instruction.generate_bytecode(*function_block, results_debug_bytes);
						results_debug_bytes_s.push_back(util::bytes_to_str(results_debug_bytes));
						results_debug_bytes_pos_s.push_back(util::num_to_str(bytes_pos));
						bytes_pos += results_debug_bytes.size();
						max_length = results.back().size() > max_length ? results.back().size() : max_length;
						max_length_bytes = results_debug_bytes_s.back().size() > max_length_bytes ? results_debug_bytes_s.back().size() : max_length_bytes;
						max_length_bytes_pos = results_debug_bytes_pos_s.back().size() > max_length_bytes_pos ? results_debug_bytes_pos_s.back().size() : max_length_bytes_pos;
					});
				}
			}

			for (size_t i = 0; i < results.size(); ++i)
			{
				if (results[i].starts_with("mark"))
				{
					result +=
						results[i]
						+ "\n";
				}
				else
				{
					result +=
						"    "
						_debug_(+ std::string(max_length_bytes_pos - results_debug_bytes_pos_s[i].size(), ' ') + results_debug_bytes_pos_s[i] + "   ")
						_debug_(+ results_debug_bytes_s[i] + std::string(max_length_bytes - results_debug_bytes_s[i].size() + 3, ' '))
						+ results[i]
						_debug_(+ std::string(max_length - results[i].size() + 3, ' ') + "; " + results_debug[i])
						+ "\n";
				}
			}
		}

		return result;
	}
};


} // namespace vm



void mylang_byte_main(int argc, const char **argv)
{
	std::string text = util::read_file("../src/mylang/mylang-byte_test.txt");

	const char *s = text.data();
	const char *e = s + text.size();

	auto root = mylang::$parse_root(s, e).value();

	// std::cout << mylang::helpers::generate_graphviz(root);

	int b = 0;

	{
		std::cout << "--- code begin ---\n" << mylang::helpers::ansii_colored(root, ansii_colors, default_ansii_color) << "--- code end ---\n";
	}

	{
		vm::fix_tree(root);
		vm::optimize_tree(root);

		vm::Compiler compiler;
		compiler.compile_root(root);

		vm::Program program = compiler.generate_program();

		std::cout << "--- bytecode begin ---\n";
		std::cout << compiler.to_string_bytecode();
		std::cout << "--- bytecode end ---\n";

		std::cout << "--- program begin ---\n";
		program.run();
		std::cout << "--- program end ---\n";
		std::cout << "--- my_update begin ---\n";
		program.run_call({ "exports", "my_update" }, { *program.resolve({ "std", "print" }), sptr<vm::RuntimeValueI64>::create_bounded(50) });
		std::cout << "--- my_update end ---\n";
	}


	int a = 0;
}

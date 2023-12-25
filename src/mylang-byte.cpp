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


void fix_tree(mylang::$Parsed &par)
{
	if (par.identifier == mylang::$IdentifierType::$i_str)
	{
		auto &str = par;
		auto &str_$g0 = str.group[1];

		str_$g0.literal = replace_str(str_$g0.flatten(), "\\\"", "\"");
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


namespace vm
{


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

struct RuntimeValueLambda : IRuntimeValueBaseTemplated<ERuntimeValueType::Lambda>
{
	using lambda_t = std::function<void(ivalue_t &result, const std::span<ivalue_t> &args)>;

	lambda_t val;
	size_t stack_size = 0;
	size_t code_offset = (size_t)-1;

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

enum class InstructionOpCode : uint8_t
{
	NOP,

	RET,
	CALL,
	MOV,
	DGET,
	ADD,

	HLT,
};

struct Program
{
	struct RetInfo
	{
		size_t code_offset;
		size_t stack_size;
		size_t result_stack_offset;
	};

	std::vector<uint8_t> bytes;
	std::vector<RetInfo> ret_stack;
	std::vector<ivalue_t> stack;
	std::vector<ivalue_t> statics;

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
			b += 2;
			memcpy(&id, b, 2);
		}
		else
		if (packed == 0b0111'1110)
		{
			b += 4;
			memcpy(&id, b, 4);
		}
		else
		{
			b += 8;
			assert(packed == 0b0111'1111);
			memcpy(&id, b, 8);
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

	void run(size_t code_offset, size_t stack_size)
	{
		stack.resize(stack_size);

		const uint8_t *b = bytes.data();

		b += code_offset;

		while (42)
		{
			InstructionOpCode opcode = (InstructionOpCode)*b++;

			switch (opcode)
			{
				case InstructionOpCode::NOP:
					break;


				case InstructionOpCode::RET:
					{
						{
							auto &last_ret = ret_stack.back();

							stack[last_ret.result_stack_offset] = unpack_value(b);
							b = bytes.data() + last_ret.code_offset;
							stack.resize(last_ret.stack_size);
						}

						ret_stack.pop_back();
						break;
					}

				case InstructionOpCode::CALL:
					{
						auto func_pair = unpack_number(b);
						auto result_pair = unpack_number(b);
						auto num_args_pair = unpack_number(b);

						assert(result_pair.second == 0);
						assert(num_args_pair.second == 0);
						size_t prev_size = stack.size();
						size_t num_args = num_args_pair.first;

						stack.resize(prev_size + num_args);

						for (size_t i = 0; i < num_args; ++i)
							stack[prev_size + i] = unpack_value(b, num_args);

						size_t result_stack_offset = &unpack_value(result_pair, num_args) - stack.data();
						const auto &func = unpack_value(func_pair, num_args);

						assert(func->type == ERuntimeValueType::Lambda);

						const auto &lambda = func.casted<RuntimeValueLambda>();

						if (lambda->code_offset == (size_t)-1)
						{
							assert(lambda->val);
							lambda->val(stack[result_stack_offset], std::span<ivalue_t>{ stack.data() + prev_size, num_args } );
							stack.resize(prev_size);
						}
						else
						{
							ret_stack.push_back(RetInfo{
								.code_offset = (size_t)(b - bytes.data()),
								.stack_size = prev_size,
								.result_stack_offset = result_stack_offset,
							});

							b = bytes.data() + lambda->code_offset;
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


				case InstructionOpCode::HLT:
					return;

				default:
					throw 1;
			}
		}
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

	using VarIDType_Base = std::variant<
		LocalVariableID,
		StaticVariableID
	>;

	struct VarIDType : VarIDType_Base
	{
		template <typename T>
		VarIDType(const T &v) : VarIDType_Base(v)
		{
		}

		std::string to_string_bytecode() const
		{
			const auto &visitor = [&](const auto &val) {
				return val.to_string_bytecode();
			};

			return std::visit(visitor, *this);
		}
	};

	struct FunctionBlock;

	struct LambdaInfo
	{
		FunctionBlock *function_block;
	};

	using StaticType = std::variant<
		int64_t,
		std::string,
		LambdaInfo
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
		const auto &visitor = [&](const auto &instr) {
			return instr.id;
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

	struct Instruction_RET
	{
		VarIDType result;

		_debug_(std::string _debug_info;);

		void generate_bytecode(std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::RET);
			pack_number(bytecode, result);
		}

		std::string to_string_bytecode() const
		{
			std::string res;

			res += "ret ";
			res += result.to_string_bytecode() + " ";

			return res;
		}
	};

	struct Instruction_CALL
	{
		VarIDType func;
		VarIDType result;
		std::vector<VarIDType> args;

		_debug_(std::string _debug_info;);

		void generate_bytecode(std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::CALL);
			pack_number(bytecode, func);
			pack_number(bytecode, result);
			pack_number(bytecode, LocalVariableID{ args.size() });

			for (const auto &arg : args)
				pack_number(bytecode, arg);
		}

		std::string to_string_bytecode() const
		{
			std::string res;

			res += "call ";
			res += func.to_string_bytecode() + " ";
			res += result.to_string_bytecode() + " ";

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

		void generate_bytecode(std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::MOV);
			pack_number(bytecode, dest);
			pack_number(bytecode, src);
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

	struct Instruction_ADD
	{
		VarIDType result;
		VarIDType left;
		VarIDType right;

		_debug_(std::string _debug_info;);

		void generate_bytecode(std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::ADD);
			pack_number(bytecode, result);
			pack_number(bytecode, left);
			pack_number(bytecode, right);
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

	struct Instruction_DGET
	{
		VarIDType result;
		VarIDType dict;
		VarIDType path;

		_debug_(std::string _debug_info;);

		void generate_bytecode(std::vector<uint8_t> &bytecode) const
		{
			bytecode.push_back((uint8_t)InstructionOpCode::DGET);
			pack_number(bytecode, result);
			pack_number(bytecode, dict);
			pack_number(bytecode, path);
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

	struct Instruction_HLT
	{
		_debug_(std::string _debug_info;);

		void generate_bytecode(std::vector<uint8_t> &bytecode) const
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
		Instruction_RET,
		Instruction_CALL,
		Instruction_MOV,
		Instruction_ADD,
		Instruction_DGET,
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

		void generate_bytecode(std::vector<uint8_t> &bytecode) const
		{
			const auto &visitor = [&](const auto &instr) {
				return instr.generate_bytecode(bytecode);
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

	struct FunctionBlock
	{
		std::vector<InstructionType> instructions;
		size_t stack_size = 0;
		size_t code_offset = (size_t)-1;
		_debug_(std::string _debug_func_name;);
	};

	struct Stack
	{
		std::vector<std::unordered_map<std::string, LocalVariableID>> locals;
		FunctionBlock *function_block;
	};

	std::vector<StaticType> statics;
	std::vector<Stack> stacks;
	std::vector<std::unique_ptr<FunctionBlock>> function_blocks;

	void push_stack()
	{
		stacks.emplace_back();
		push_stack_locals();
		stacks.back().function_block = function_blocks.emplace_back(std::make_unique<FunctionBlock>()).get();
	}

	void pop_stack()
	{
		assert(stacks.size() > 0);
		stacks.pop_back();
	}

	void push_stack_locals()
	{
		assert(stacks.size() > 0);
		stacks.back().locals.emplace_back();
	}

	void pop_stack_locals()
	{
		assert(stacks.size() > 0);
		stacks.back().locals.pop_back();
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
	LocalVariableID add_local_var()
	{
		assert(stacks.size() > 0);
		return { stacks.back().function_block->stack_size++ };
	}

	void add_instruction(InstructionType instruction)
	{
		stacks.back().function_block->instructions.push_back(instruction);
	}

	void declare_local(const std::string &token, LocalVariableID id)
	{
		stacks.back().locals.back()[token] = id;
	}

	std::optional<LocalVariableID> find_local(const std::string &tok)
	{
		auto &locals = stacks.back().locals;

		for (auto it = locals.rbegin(); it != locals.rend(); ++it)
		{
			if (auto tit = it->find(tok); tit != it->end())
			{
				return { tit->second };
			}
		}

		return std::nullopt;
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

					auto v_expr_left = compile_expr(expr_left);

					// TODO;

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
						for (size_t i = 0; i < expr_add_$g0.size(); ++i)
						{
							const auto &expr_add_$g0_$g0 = expr_add_$g0.get(i, mylang::$IdentifierType::$i_expr_add_$g0_$g0);
							const auto &expr_add_$g0_$g0_$g0 = expr_add_$g0_$g0.get(1, mylang::$IdentifierType::$i_expr_add_$g0_$g0_$g0);
							const auto &expr_right = expr_add_$g0_$g0.get(3);

							auto v_expr_right = compile_expr(expr_right);

							if (expr_add_$g0_$g0_$g0.get(0).literal == "+")
							{
								auto v_result = add_local_var();

								add_instruction(Instruction_ADD{
									.result = v_result,
									.left = v_expr_left,
									.right = v_expr_right,
									_debug_(._debug_info = minimize_whitespaces(mylang::helpers::ansii_colored(expr_add, ansii_colors, default_ansii_color)))
								});

								v_expr_left = v_result;
							}
							else
							if (expr_add_$g0_$g0_$g0.get(0).literal == "-")
							{
								auto v_result = add_local_var();
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
			case mylang::$IdentifierType::$i_token_path:
				{
					const auto &token_path = par;
					const auto &token = token_path.get(0, mylang::$IdentifierType::$i_token);
					const auto &token_path_$g0 = token_path.get(1, mylang::$IdentifierType::$i_token_path_$g0);

					std::string tok = token.flatten();

					VarIDType v_result = StaticVariableID{ 0 };

					if (auto v_value = find_local(tok))
					{
						v_result = v_value.value();
					}
					else
					{
						auto v_result_dget = add_local_var();
						auto v_path = add_static_str(tok);

						add_instruction(Instruction_DGET{
							.result = v_result_dget,
							.dict = StaticVariableID{ 0 }, // search for unresolved links in global dict
							.path = v_path,
							_debug_(._debug_info = minimize_whitespaces(mylang::helpers::ansii_colored(token, ansii_colors, default_ansii_color)))
						});

						v_result = v_result_dget;
					}

					if (token_path_$g0.size() > 0)
					{
						for (size_t i = 0; i < token_path_$g0.size(); ++i)
						{
							const auto &token_path_$g0_$g0 = token_path_$g0.get(i, mylang::$IdentifierType::$i_token_path_$g0_$g0);
							const auto &token = token_path_$g0_$g0.get(3, mylang::$IdentifierType::$i_token);

							std::string tok = token.flatten();

							auto v_new_result = add_local_var();
							auto v_path = add_static_str(tok);

							add_instruction(Instruction_DGET{
								.result = v_new_result,
								.dict = v_result,
								.path = v_path,
								_debug_(._debug_info = minimize_whitespaces(mylang::helpers::ansii_colored(token, ansii_colors, default_ansii_color)))
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

					auto v_result = add_local_var();

					add_instruction(Instruction_CALL{
						.func = v_func,
						.result = v_result,
						.args = std::move(v_args),
						_debug_(._debug_info = minimize_whitespaces(mylang::helpers::ansii_colored(expr_call, ansii_colors, default_ansii_color)))
					});

					return v_result;
				}
			case mylang::$IdentifierType::$i_expr_function:
				{
					const auto &expr_function = par;
					const auto &expr_function_$g0 = expr_function.get(2, mylang::$IdentifierType::$i_expr_function_$g0);
					const auto &expr_block = expr_function.get(5, mylang::$IdentifierType::$i_expr_block);

					std::vector<std::string> capture_tokens;
					std::vector<std::string> ignore_tokens;
					collect_captures(expr_function, capture_tokens, ignore_tokens);

					// TODO: captures

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

					push_stack();

					ExitScope _onexit([&]() {
						pop_stack();
					});

					for (const auto &t : capture_tokens)
						declare_local(t, add_local_var());

					for (const auto &t : args_tokens)
						declare_local(t, add_local_var());

					auto v_ret = compile_expr_block(expr_block);

					add_instruction(Instruction_RET{
						.result = v_ret,
					});

					return add_static_lambda(LambdaInfo {
						.function_block = stacks.back().function_block,
					});
				}
			case mylang::$IdentifierType::$i_expr_block:
				{
					const auto &expr_block = par;

					push_stack_locals();

					ExitScope _onexit([&]() {
						pop_stack_locals();
					});

					return compile_expr_block(expr_block);
				}
		}

		throw 1;
	}

	VarIDType compile_expr_block(const mylang::$Parsed &expr_block)
	{
		assert(expr_block.identifier == mylang::$IdentifierType::$i_expr_block);

		const auto &expr_block_$g0 = expr_block.get(2, mylang::$IdentifierType::$i_expr_block_$g0);
		const auto &expr_block_$g1 = expr_block.get(3, mylang::$IdentifierType::$i_expr_block_$g1);

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

		return StaticVariableID{ 0 }; // TODO: return null
	}

	void compile_statement_let(const mylang::$Parsed &statement_let)
	{
		assert(statement_let.identifier == mylang::$IdentifierType::$i_statement_let);

		const auto &token = statement_let.get(3, mylang::$IdentifierType::$i_token);
		const auto &expr = statement_let.get(7);

		auto v_expr = compile_expr(expr);
		auto v_token = add_local_var();

		_debug_({
			if (const auto *static_v_expr = std::get_if<StaticVariableID>(&v_expr))
			{
				if (auto *lambda_v_expr = std::get_if<LambdaInfo>(&statics[static_v_expr->id]))
				{
					lambda_v_expr->function_block->_debug_func_name = token.flatten();
				}
			}
		});

		declare_local(token.flatten(), v_token);
		add_instruction(Instruction_MOV{
			.dest = v_token,
			.src = v_expr,
			_debug_(._debug_info = minimize_whitespaces(mylang::helpers::ansii_colored(statement_let, ansii_colors, default_ansii_color)))
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
			case mylang::$IdentifierType::$i_statement_let:
				{
					const auto &statement_let = par;
					return compile_statement_let(statement_let);
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

		auto v_global_dict = add_static_i64(0);
		(void)v_global_dict;

		push_stack();

		ExitScope _onexit([&]() {
			assert(stacks.size() == 1 && stacks[0].locals.size() == 1);

			pop_stack();
		});

		_debug_(function_blocks[0]->_debug_func_name = "_start");

		const auto &root_$g0 = root.get(1, mylang::$IdentifierType::$i_root_$g0);

		for (size_t i = 0; i < root_$g0.size(); ++i)
		{
			const auto &root_$g0_$g0 = root_$g0.get(i, mylang::$IdentifierType::$i_root_$g0_$g0);
			const auto &statement = root_$g0_$g0.get(0);

			compile_statement(statement);
		}

		add_instruction(Instruction_HLT{});
	}

	void generate_bytecode(std::vector<uint8_t> &bytecode) const
	{
		for (const auto &function_block : function_blocks)
		{
			function_block->code_offset = bytecode.size();

			for (const auto &instruction : function_block->instructions)
				instruction.generate_bytecode(bytecode);
		}
	}

	std::string to_string_bytecode() const
	{
		std::string result;

		_debug_({
			// generate function offsets
			std::vector<uint8_t> bytes;
			generate_bytecode(bytes);
		});

		for (size_t i = 0; i < function_blocks.size(); ++i)
		{
			if (i != 0)
				result += "\n";

			const auto &function_block = function_blocks[i];

			_debug_(result += "; code offset = " + std::to_string(function_block->code_offset) + "\n");
			_debug_(result += "; stack size = " + std::to_string(function_block->stack_size) + "\n");
			_debug_(result += function_block->_debug_func_name + ":\n");

			std::vector<std::string> results;

			_debug_(size_t max_length = 0;);
			_debug_(size_t max_length_bytes = 0;);

			_debug_(std::vector<uint8_t> results_debug_bytes;);
			_debug_(std::vector<std::string> results_debug_bytes_s;);
			_debug_(std::vector<std::string> results_debug;);

			const auto &bytes_to_str = [](const std::vector<uint8_t> &bytes) -> std::string {
				const auto &byte_to_str = [](uint8_t byte) -> std::string {
					const char table[16] { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };

					std::string result(2, '0');

					result[1] = table[byte & 0xF];
					result[0] = table[byte >> 4];

					return result;
				};

				std::string result;

				for (uint8_t byte : bytes)
					result += byte_to_str(byte) + " ";

				if (result.size() > 0)
					result.resize(result.size() - 1);

				return result;
			};

			for (const auto &instruction : function_block->instructions)
			{
				results.push_back(instruction.to_string_bytecode());

				_debug_({
					results_debug.push_back(instruction._get_debug_info());
					results_debug_bytes.clear();
					instruction.generate_bytecode(results_debug_bytes);
					results_debug_bytes_s.push_back(bytes_to_str(results_debug_bytes));
					max_length = results.back().size() > max_length ? results.back().size() : max_length;
					max_length_bytes = results_debug_bytes_s.back().size() > max_length_bytes ? results_debug_bytes_s.back().size() : max_length_bytes;
				});
			}

			for (size_t i = 0; i < results.size(); ++i)
				result += "\t" + _debug_(results_debug_bytes_s[i] + std::string(max_length_bytes - results_debug_bytes_s[i].size() + 3, ' ')) + results[i] + _debug_(std::string(max_length - results[i].size() + 3, ' ') + "; " + results_debug[i]) + "\n";
		}

		return result;
	}
};


} // namespace vm



void mylang_byte_main(int argc, const char **argv)
{
	std::string text = read_file("../src/mylang/mylang-byte_test.txt");

	const char *s = text.data();
	const char *e = s + text.size();

	auto root = mylang::$parse_root(s, e).value();

	// std::cout << mylang::helpers::generate_graphviz(root);

	int b = 0;

	{
		std::cout << "--- code begin ---\n" << mylang::helpers::ansii_colored(root, ansii_colors, default_ansii_color) << "--- code end ---\n";
	}


	{
		fix_tree(root);
		optimize_tree(root);

		vm::Compiler compiler;
		compiler.compile_root(root);

		std::cout << "--- bytecode begin ---\n";
		std::cout << compiler.to_string_bytecode();
		std::cout << "--- bytecode end ---\n";

		vm::Program program;
		compiler.generate_bytecode(program.bytes);

		{
			// static with index 0 is global dict
			auto g = sptr<vm::RuntimeValueDict>::create_bounded();
			program.statics.push_back(g);

			auto std = sptr<vm::RuntimeValueDict>::create_bounded();
			g->val["std"] = std;

			std->val["print"] = sptr<vm::RuntimeValueLambda>::create_bounded([](vm::ivalue_t &result, const std::span<vm::ivalue_t> &args) {

				for (size_t i = 0; i < args.size(); ++i)
				{
					const auto &arg = args[i];

					if (i != 0)
						std::cout << " ";

					switch (arg->type)
					{
						case vm::ERuntimeValueType::I64:
							std::cout << arg.casted<vm::RuntimeValueI64>()->val;
							break;
						case vm::ERuntimeValueType::Str:
							std::cout << arg.casted<vm::RuntimeValueStr>()->val;
							break;
						case vm::ERuntimeValueType::Dict:
								std::cout << "dict{ size = " << arg.casted<vm::RuntimeValueDict>()->val.size() << " }";
							break;
						case vm::ERuntimeValueType::Lambda:
								std::cout << "<lambda>";
							break;
					}
				}

				std::cout << "\n";
			});
		}

		{
			for (size_t i = 1; i < compiler.statics.size(); ++i)
			{
				const auto &visitor = [&](const auto &static_var) {
					using T = std::decay_t<decltype(static_var)>;

					if constexpr (std::is_same_v<T, int64_t>)
					{
						program.statics.push_back(sptr<vm::RuntimeValueI64>::create_bounded(static_var));
					}
					else
					if constexpr (std::is_same_v<T, std::string>)
					{
						program.statics.push_back(sptr<vm::RuntimeValueStr>::create_bounded(static_var));
					}
					else
					if constexpr (std::is_same_v<T, vm::Compiler::LambdaInfo>)
					{
						auto lambda = sptr<vm::RuntimeValueLambda>::create_bounded(nullptr);
						lambda->stack_size = static_var.function_block->stack_size;
						lambda->code_offset = static_var.function_block->code_offset;
						program.statics.push_back(lambda);
					}
					else
					{
						static_assert(vm::always_false_v<T>, "Unknown type");
					}
				};

				std::visit(visitor, compiler.statics[i]);
			}
		}

		std::cout << "--- program begin ---\n";
		program.run(compiler.function_blocks[0]->code_offset, compiler.function_blocks[0]->stack_size);
		std::cout << "--- program end ---\n";
	}


	int a = 0;
}

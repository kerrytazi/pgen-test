
#include "gen/test.hpp"

namespace test::helpers
{


std::string generate_tree_simple(const $$Parsed &p, size_t align = 0)
{
	std::string result;

	if (p.type == $$ParsedType::Literal)
		result += std::string(align, ' ') + "'" + std::string(p.literal) + "'\n";

	if (p.type == $$ParsedType::Identifier || p.type == $$ParsedType::Group)
	{
		int new_align = align;

		if (p.group.size() > 1)
			new_align += 1;

		for (const auto &v : p.group)
			result += generate_tree_simple(v, new_align);
	}

	return result;
}

} // namespace test::helpers

#include <iostream>
#include <cassert>

namespace test_
{

struct State
{
	std::unordered_map<std::string, uint64_t> regs;
	// std::unordered_map<std::string, uint64_t> marks;
};

void evaluate2(const test::$$Parsed &p, State &state)
{
	if (p.identifier == "root")
	{
		if (p.group.size() > 0)
		{
			// root -> root_$g0 -> statement
			evaluate2(p.group[0].group[0], state);

			// root -> root_$g0 -> root_$g0_$g0 -> statement
			for (size_t i = 1; i < p.group[0].group.size(); ++i)
				evaluate2(p.group[0].group[i].group[0], state);
		}
	}
	else
	if (p.identifier == "statement")
	{
		evaluate2(p.group[0], state);
	}
	else
	if (p.identifier == "mark")
	{
		// TODO: marks
	}
	else
	if (p.identifier == "instruction")
	{
		if (p.group[0].identifier == "instruction_regrm")
		{
			evaluate2(p.group[0], state);
		}
		else
		if (p.group[0].identifier == "instruction_jmp")
		{
			// TODO: marks
		}
	}
	else
	if (p.identifier == "instruction_regrm")
	{
		assert(p.group[1].group.size() == 2);

		struct Argument
		{
			std::string reg;
			std::string rm_base;
			std::string rm_index;
			int rm_scale = 1;
			int rm_displacement = 0;
		};

		const auto parse_arg = [&](const test::$$Parsed &arg) -> Argument {
			Argument result;

			if (arg.group[0].identifier == "kv_reg")
			{
				// argument -> kv_reg -> literal
				result.reg = arg.group[0].group[0].literal;
			}
			else
			{
				// argument -> addr_expr -> kv_reg
				if (arg.group[0].group[1].identifier == "kv_reg")
				{
					if (auto g = arg.group[0].find("addr_expr_$g0"))
					{
						// argument -> addr_expr -> kv_reg -> literal
						result.rm_base = arg.group[0].group[1].group[0].literal;

						// addr_expr_$g0 -> kv_reg -> literal
						result.rm_index = g->group[1].group[0].literal;
					}

					if (auto g = arg.group[0].find("addr_expr_$g1"))
					{
						if (result.rm_base.empty())
						{
							// argument -> addr_expr -> kv_reg -> literal
							result.rm_index = arg.group[0].group[1].group[0].literal;
						}

						// addr_expr_$g1 -> number
						result.rm_scale = atoi(g->group[1].flatten().c_str());
					}

					if (auto g = arg.group[0].find("addr_expr_$g2"))
					{
						// addr_expr_$g2 -> number
						result.rm_displacement = atoi(g->group[1].flatten().c_str());

						if (g->group[0].group[0].literal == "-")
							result.rm_displacement = -result.rm_displacement;
					}
				}
				else
				{
					// TODO: displacement only
				}
			}

			return result;
		};

		const auto parse_rm = [&](const Argument &arg) -> uint64_t
		{
			uint64_t v = 0;

			if (!arg.rm_base.empty())
				v += state.regs.at(arg.rm_base);

			if (!arg.rm_index.empty())
				v += state.regs.at(arg.rm_index) * arg.rm_scale;

			v += arg.rm_displacement;

			return v;
		};

		// instruction_regrm -> instruction_regrm_$g1 -> argument
		auto arg0 = parse_arg(p.group[1].group[0]);

		// instruction_regrm -> instruction_regrm_$g1 -> instruction_regrm_$g1_$g0 -> argument
		auto arg1 = parse_arg(p.group[1].group[1].group[1]);

		if (p.group[0].group[0].literal == "mov")
		{
			assert(!(arg0.reg.empty() && arg1.reg.empty()));

			if (arg0.reg.empty())
			{
				*(uint64_t *)parse_rm(arg0) = state.regs.at(arg1.reg);
			}
			else
			{
				if (arg1.reg.empty())
				{
					state.regs.at(arg0.reg) = *(uint64_t *)parse_rm(arg1);
				}
				else
				{
					state.regs.at(arg0.reg) = state.regs.at(arg1.reg);
				}
			}
		}
		else
		if (p.group[0].group[0].literal == "add")
		{
		}
		else
		if (p.group[0].group[0].literal == "sub")
		{
		}
	}
}

void test1()
{
	std::string_view text = R"AAA(
loop:
mov rax, rbx
mov rbx, [rax + rcx]
mov rbx, [rcx * 4]
mov rbx, [rcx * 4 + 2344]
mov rbx, [rax + rcx * 4 + 2344]
jmp loop
)AAA";

	const char *s = text.data();
	const char *e = s + text.size();

	auto result = test::$parse_root(s, e);

	// std::cout << test::helpers::flatten(result.value().group[0].group[0].group[0].group[0]) << "\n";

	// std::cout << test::helpers::generate_graphviz(result.value());
	// std::cout << test::helpers::generate_tree(result.value());

	State state;
	state.regs["rax"] = 0;
	state.regs["rbx"] = 0;
	state.regs["rcx"] = 0;
	state.regs["rdx"] = 0;
	evaluate2(result.value(), state);

	int a = 0;
}

int evaluate(const test::$$Parsed &p)
{
	if (p.identifier == "math_expr")
	{
		return evaluate(p.group[0]);
	}
	else
	if (p.identifier == "math_expr_add")
	{
		int r = evaluate(p.group[0]);

		for (size_t i = 1; i < p.group.size(); ++i)
		{
			if (p.group[i].group[0].flatten() == "+")
			{
				r += evaluate(p.group[i].group[1]);
			}
			else
			// if (test::helpers::flatten(p.group[i].group[0]) == "-")
			{
				r -= evaluate(p.group[i].group[1]);
			}
		}

		return r;
	}
	else
	if (p.identifier == "math_expr_mul")
	{
		int r = evaluate(p.group[0]);

		for (size_t i = 1; i < p.group.size(); ++i)
		{
			if (p.group[i].group[0].flatten() == "*")
			{
				r *= evaluate(p.group[i].group[1]);
			}
			else
			// if (test::helpers::flatten(p.group[i].group[0]) == "/")
			{
				r /= evaluate(p.group[i].group[1]);
			}
		}

		return r;
	}
	else
	if (p.identifier == "math_expr_primitive")
	{
		if (p.group[0].identifier == "number")
		{
			return std::atoi(p.group[0].flatten().c_str());
		}
		else
		if (p.group[0].identifier == "token")
		{
			// TODO
			return 0;
		}
		else
		// round brackets
		{
			return evaluate(p.group[1]);
		}
	}

	return 0;
}

void test2()
{
	std::string_view text = R"AAA(3 + 4 * 5 + 6)AAA";

	/*

	std::string_view text = R"AAA(
3 + 4 * 5 + 6
)AAA";
2342;
a + b;
a + b * c + d;
a * (b + c) + d;
	*/

	const char *s = text.data();
	const char *e = s + text.size();

	auto result = test::$parse_math_expr(s, e);

	// std::cout << test::helpers::flatten(result.value().group[0].group[0].group[0].group[0]) << "\n";

	std::cout << result.value().flatten() << " = " << evaluate(result.value()) << "\n";
	// std::cout << test::helpers::generate_graphviz(result.value());
	// std::cout << test::helpers::generate_tree_simple(result.value());

	int a = 0;
}


} // namespace test_

extern void mylang_main(int argc, const char **argv);

int main(int argc, const char **argv)
{
	// test_::test1();
	// test_::test2();
	mylang_main(argc, argv);
}

import re
import sys

from sympy import solve, symbols

sys.setrecursionlimit(10000)


def resolve_monkey(monkeys, expression):
    monkeys_expr = re.findall(r"\w{4}", expression)
    if monkeys_expr != []:
        if monkeys_expr[0] in monkeys:
            expression = resolve_monkey(
                monkeys, expression.replace(monkeys_expr[0], monkeys[monkeys_expr[0]])
            )
        elif len(monkeys_expr) > 1:
            expression = resolve_monkey(
                monkeys, expression.replace(monkeys_expr[1], monkeys[monkeys_expr[1]])
            )

    return expression


with open("input.txt", "r") as f:
    monkeys = {}
    for l in f.readlines():
        m = re.match(r"(\w+): (-?\d+)", l)
        if m:
            monkeys[m.groups()[0]] = m.groups()[1]
        else:
            m = re.match(r"(\w+): (.*)", l)
            monkeys[m.groups()[0]] = "(" + m.groups()[1] + ")"

print("Question 1:", int(eval(resolve_monkey(monkeys, monkeys["root"]))))

del monkeys["humn"]
del monkeys["root"]

left = resolve_monkey(monkeys, monkeys["jhpn"])
right = resolve_monkey(monkeys, monkeys["jmsg"])

humn = solve([left + f" -{int(eval(right))}"], dict=True)[0][symbols("humn")]
print("Question 2:", humn)

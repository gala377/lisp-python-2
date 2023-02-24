from dataclasses import dataclass, field
from typing import Any, Iterator, MutableMapping, MutableSequence, Optional, Sequence


class Sym:
    __symbols__: MutableMapping[str, "Sym"] = {}

    def __new__(cls, val):
        if inst := cls.__symbols__.get(val):
            return inst
        inst = super().__new__(cls)
        cls.__symbols__[val] = inst
        return inst

    def __init__(self, val: str):
        self._val = val

    def __str__(self):
        return self._val

    def __repr__(self):
        return f's"{self._val}"'

    def __eq__(self, other):
        return self is other

    def __hash__(self):
        return hash(id(self))


def parse(lexems: MutableSequence[str]) -> Iterator[Any]:
    while lexems:
        lex = lexems.pop(0)
        match lex:
            case "(":
                acc = []
                while lexems[0] != ")":
                    acc.append(next(parse(lexems)))
                lexems.pop(0)
                yield acc
            case ")":
                raise SyntaxError("unexpected closing parenthesis")
            case "'":
                yield [Sym("quote"), next(parse(lexems))]
            case _:
                yield _atom(lex)


def tokenize(src: str) -> Sequence[str]:
    return src.replace("(", " ( ").replace(")", " ) ").replace("'", " ' ").split()


def _atom(lexeme: str) -> Any:
    if lexeme[0] == '"' and lexeme[-1] == '"':
        return lexeme[1:-1]
    try:
        return int(lexeme)
    except ValueError:
        try:
            return float(lexeme)
        except ValueError:
            return Sym(lexeme)


@dataclass
class Env:
    bindings: dict[Sym, Any] = field(default_factory={})
    parent: Optional["Env"] = None

    def set(self, sym: Sym, val: Any):
        self.bindings[sym] = val

    def lookup(self, sym: Sym) -> Optional[Any]:
        if (val := self.bindings.get(sym)) is not None:
            return val
        if not self.parent:
            raise KeyError(f"name '{sym}' is undefined")
        return self.parent.lookup(sym)

    def copy_self(self) -> "Env":
        if not self.parent:
            # global env, just return a reference
            return self
        parent = self.parent.copy_self()
        bindings = self.bindings.copy()
        Env(bindings, parent)

    def top_env(self) -> "Env":
        if not self.parent:
            return self
        return self.parent.top_env()

def error(_, msg):
    raise ValueError(msg)

STANDARD_BINDINGS = {
    Sym(s): val
    for s, val in {
        "print": lambda _, x: print(x),
        "repr": lambda _, x: repr(x),
        "+": lambda _, *args: sum(args),
        "-": lambda _, x, y: x - y,
        "*": lambda _, x, y: x * y,
        "/": lambda _, x, y: x / y,
        "eq?": lambda _, x, y: x == y,
        "car": lambda _, l: l[0],
        "cdr": lambda _, l: l[1:],
        "cons": lambda _, a, b: [a] + b,
        "list": lambda _, *args: list(args),
        "list?": lambda _, x: isinstance(x, list),
        "nil?": lambda _, x: x == [],
        "symbol?": lambda _, x: isinstance(x, Sym),
        "error": error,
        "eval": lambda env, expr: eval(env, expr),
    }.items()
}


def eval_str(src: str) -> Any:
    [expr] = (x for x in parse(tokenize(src)))
    env = Env(bindings=STANDARD_BINDINGS)
    return eval(env, expr)


def eval(env: Env, expr: Any) -> Any:
    match expr:
        case []:
            return []
        case [Sym() as s, *args]:
            if (ret := eval_special_form(env, s, *args)) is not None:
                return ret
            f = eval(env, s)
            if isinstance(f, Sym):
                raise ValueError(f"Symbol {f} is not callable")
            ret = f(env.top_env(), *[eval(env, x) for x in args])
            if ret is None:
                return []
            return ret
        case [func, *args]:
            f = eval(env, func)
            ret = f(env.top_env(), *[eval(env, x) for x in args])
            if ret is None:
                return []
            return ret
        case Sym() as s:
            return env.lookup(s)
        case val:
            return val


def eval_special_form(env: Env, form: Sym, *args: Any) -> Optional[Any]:
    if form == Sym("define"):
        match args:
            case [Sym() as name, val]:
                env.set(name, eval(env, val))
                return []
            case _:
                raise ValueError("Wrong define form")
    if form == Sym("begin"):
        if not args:
            return []
        return [eval(env, expr) for expr in args][-1]
    if form == Sym("quote"):
        [val] = args
        return val
    if form == Sym("cond"):
        for arm in args:
            match arm:
                case [cond, expr]:
                    if eval(env, cond):
                        return eval(env, expr)
                case _:
                    raise ValueError("wrong cond arm")
        return []
    if form == Sym("lambda"):
        match args:
            case [params, *body]:
                assert all(
                    map(lambda x: isinstance(x, Sym), params)
                ), "all lambda parameters have to names"

                lambda_env = env.copy_self()

                def lambda_func(_, *lambda_args):
                    assert len(lambda_args) == len(params), "wrong number of parameters"

                    exec_env = Env(
                        bindings=dict(zip(params, lambda_args)),
                        parent=lambda_env,
                    )
                    return eval(exec_env, [Sym("begin"), *body])

                return lambda_func

            case _:
                raise ValueError("Wrong lambda form")
    return None

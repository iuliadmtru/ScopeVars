# ScopeVars

## Usage

```julia
julia> versioninfo()
Julia Version 1.12.0-DEV.631
Commit a4e793ee31* (2024-05-30 19:25 UTC)
...

julia> using ScopeVars

julia> using JuliaLowering

julia> text = """
       function foo(a)
           b = a + 1
           # pos 1
           d = let b
               c = b
               c2 = 2c
               # pos 2
               2c2
           end
           # pos x
           x = (b, d)
           # pos 3
           return x
       end
       """;

julia> ex = JuliaLowering.parsestmt(JuliaLowering.SyntaxTree, text);

julia> in_mod = Module();

julia> pos2 = (11, 1);

julia> assignments, locals, destructured_args, globals, used_names, used_bindings = find_all_scope_vars(in_mod, ex, pos2);

julia> assignments
Set{Any} with 6 elements:
  JuliaLowering.NameKey("d", 1) => d
  JuliaLowering.NameKey("y", 1) => y
  JuliaLowering.NameKey("c", 1) => c
  JuliaLowering.NameKey("c2", 1) => c2
  JuliaLowering.NameKey("foo", 1) => foo
  JuliaLowering.NameKey("b", 1) => b

julia> locals
Set{Any} with 1 element:
  JuliaLowering.NameKey("b", 1) => (local b)

julia> used_names
Set{Any} with 8 elements:
  JuliaLowering.NameKey("+", 1) => +
  JuliaLowering.NameKey("*", 1) => *
  JuliaLowering.NameKey("b", 1) => b
  JuliaLowering.NameKey("a", 1) => a
  JuliaLowering.NameKey("c", 1) => c
  JuliaLowering.NameKey("b", 1) => b
  JuliaLowering.NameKey("a", 1) => a
  JuliaLowering.NameKey("foo", 1) => foo

julia> used_bindings
Set{Any}()
```

## Notes

There are some tests which I believe should pass but don't. I didn't
figure out why yet. I left them commented out and wrote remarks for
each.

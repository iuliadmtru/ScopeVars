using JuliaLowering
using JuliaLowering: @K_str

combine!(set_v1, set_v2) = foreach((set1, set2) -> union!(set1, set2), set_v1, set_v2)

"""
    pos_to_byte(source_file::JuliaLowering.SourceFile, pos::Tuple{Int, Int})

Convert a file position to a byte number within the file.
"""
function pos_to_byte(source_file::JuliaLowering.SourceFile, pos::Tuple{Int, Int})
    line, col = pos
    # Get the line start byte.
    line_byte = source_file.line_starts[line]
    # Find the byte corresponding to the position.
    line_str = split(source_file.code, "\n"; limit=line+1)[line]
    col_byte = collect(eachindex(line_str))[col]

    return line_byte + col_byte
end

"""
    is_at(expr::JuliaLowering.SyntaxTree, file_position::Tuple{Int, Int})

Return `true` if the given expression's span contains the given file position.
Return `false` otherwise.
"""
is_at(ex::JuliaLowering.SyntaxTree, byte::Int) = byte in JuliaLowering.byte_range(ex)

"""
    _is_internal(binding_id, ctx::JuliaLowering.AbstractLoweringContext)

Search for a binding id in a given context and check if it is internal.
"""
function _is_internal(binding_id, ctx::JuliaLowering.AbstractLoweringContext)
    bindings = filter(b -> b.id == binding_id, ctx.bindings.info)
    isempty(bindings) && return false
    return !bindings[1].is_internal
end

function find_all_scope_vars(ctx::JuliaLowering.AbstractLoweringContext,
                             ex::JuliaLowering.SyntaxTree,
                             pos::Tuple{Int, Int})
    # Get information about the variables used within the scope of `ex`.
    vars = JuliaLowering.find_scope_vars(ctx, ex)
    # Initialize arrays.
    ret = [Set() for _ in 1:length(vars)]
    # If `ex`'s span covers `pos`, store the information and continue the search among
    # its children.
    byte_pos = pos_to_byte(JuliaLowering.sourceref(ex).file, pos)
    if is_at(ex, byte_pos)
        if is_assignment(ex)
            # Exclude from `used_names` the variable that is being assigned to.
            # TODO: Should it also be excluded from `assignments`?
            used_names = vars[5]
            deleteat!(used_names,
                      findall(p -> p.second == JuliaLowering.decl_var(ex[1]), used_names))
        end
        combine!(ret, vars)
        for c in JuliaLowering.children(ex)
            c_vars = find_all_scope_vars(ctx, c, pos)
            combine!(ret, c_vars)
        end
    end

    # Remove all variables defined after `pos`.
    filter!(p -> JuliaLowering.source_location(p.second) <= pos, ret[1])  # assignments
    filter!(p -> JuliaLowering.source_location(p.second) <= pos, ret[2])  # locals
    # TODO: What about `destructured_args`?
    filter!(p -> JuliaLowering.source_location(p.second) <= pos, ret[4])  # globals
    filter!(p -> JuliaLowering.source_location(p.second) <= pos, ret[5])  # used_names
    # TODO: Is this correct?
    filter!(id -> _is_internal(id, ctx), ret[6])                          # used_bindings

    return ret
end

"""
    find_all_scope_vars(mod::Module, expr::JuliaLowering.SyntaxTree, pos::Tuple{Int, Int})

Find all the variables used in the scope found at `pos` within `expr`. Return a tuple of
arrays, similar to `JuliaLowering.find_scope_vars`.

Should be used as:
```julia
vars = find_all_scope_vars(mod, expr, (line, col))
assignments, locals, destructured_args, globals, used_names, used_bindings = vars
```
"""
function find_all_scope_vars(mod::Module, ex::JuliaLowering.SyntaxTree, pos::Tuple{Int, Int})
    # The documentation on `JuliaLowering.find_scope_vars` mentions that it works best
    # after desugaring. However, each lowering step adds to the context (the contexts are
    # dependent on each other). It might be best to go through all lowering steps. It does
    # take a longer time though, and the results seem to be the same when lowering only up
    # to (and including) desugaring.
    ctx1, ex_macroexpand = JuliaLowering.expand_forms_1(mod, ex)
    ctx2, ex_desugar = JuliaLowering.expand_forms_2(ctx1, ex_macroexpand)
    ctx3, ex_scoped = JuliaLowering.resolve_scopes(ctx2, ex_desugar)
    ctx4, ex_converted = JuliaLowering.convert_closures(ctx3, ex_scoped)
    JuliaLowering.linearize_ir(ctx4, ex_converted)

    return find_all_scope_vars(ctx2, ex_desugar, pos)
end

## -------------------------------------------

## Syntax utils

is_assignment(ex::JuliaLowering.SyntaxTree) = ex.kind === K"="

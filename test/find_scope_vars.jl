include("utils.jl")

@testset "Find scope variables" begin
    src1 = """
    const a = 2
    function f(x)
        # pos1 = (3, 1)
        global a
        # pos2 = (5, 1)
        # pos3 = (8, 13)
        # -------v
        return a + x
        # pos4 = (9, 1)
    end
    """
    ex1 = JuliaLowering.parseall(JuliaLowering.SyntaxTree, src1)
    source_file1 = JuliaLowering.sourceref(ex1).file

    src2 = """
    function tεst_leΤ_χ()
        # pos1 = (2, 0) -- error
        x = 0
        map(1:3) do _
            x += 1
            # pos2 = (6, 1)
            let x = x
                # pos3 = (8, 1)
                return ()->x
            end
        end
        # pos4 = (12, 100) -- error
    end

    # pos5 = (17, 12)
    # ----------v
    [f() for f in tεst_leΤ_χ()]
    """
    ex2 = JuliaLowering.parseall(JuliaLowering.SyntaxTree, src2)
    source_file2 = JuliaLowering.sourceref(ex2).file

    src3 = """
    function foo(a)
        y, _ = let
            aa = 1
            aa, a
            # pos1 = (5, 5)
        end
        map([A, B, C]) do x
            # pos2 = (8, 1)
            if x < 0 && iseven(x)
                return 0
            elseif x == 0
     		local z = 2
                # pos3 = (15, 23)
                # ---------v
                return z + y
            else
                return x
            end
        end
        # pos4 = (20, 10)
    end
    """
    ex3 = JuliaLowering.parsestmt(JuliaLowering.SyntaxTree, src3)
    source_file3 = JuliaLowering.sourceref(ex3).file

    src4 = """
    function foo(a)
        b = a + 1
        # pos1 = (3, 1)
        d = let b = 1
            c = b
            c2 = 2c
            # pos2 = (7, 1)
            2c2
        end
        # pos3 = (10, 1)
        global d
        x = (b, d)
        # pos4 = (13, 1)
        return x
    end
    """
    ex4 = JuliaLowering.parsestmt(JuliaLowering.SyntaxTree, src4)
    source_file4 = JuliaLowering.sourceref(ex4).file

    @testset "Position" begin
        ## src1
        pos1 = (3, 1)
        byte1 = pos_to_byte(source_file1, pos1)
        @test byte1 == ncodeunits("const a = 2\nfunction f(x)\n ") + 1
        @test is_at(ex1, byte1)
        @test is_at(ex1[2], byte1)                              # function
        @test is_at(ex1[2][2], byte1)                           # function body
        @test !is_at(ex1[2][2][1], byte1)                       # (global a)
        pos2 = (5, 1)
        byte2 = pos_to_byte(source_file1, pos2)
        @test is_at(ex1, byte2)
        @test is_at(ex1[2][2], byte2)                           # function body
        @test !is_at(ex1[2][2][1], byte2)                       # (global a)
        pos3 = (8, 13)
        byte3 = pos_to_byte(source_file1, pos3)
        @test is_at(ex1[2][2], byte3)                           # function body
        @test is_at(ex1[2][2][2], byte3)                        # (return (call a + x))
        @test is_at(ex1[2][2][2][1], byte3)                     # (call a + x)
        @test !is_at(ex1[2][2][2][1][1], byte3)                 # (a)
        @test is_at(ex1[2][2][2][1][2], byte3)                  # (+)
        @test !is_at(ex1[2][2][2][1][3], byte3)                 # (x)
        pos4 = (9, 1)
        byte4 = pos_to_byte(source_file1, pos4)
        @test is_at(ex1[2][2], byte4)                           # function body
        @test !is_at(ex1[2][2][2], byte4)                       # (return (call a + x))

        ## src2
        pos1_err = (2, 0)
        @test_throws BoundsError pos_to_byte(source_file2, pos1_err)
        pos1 = (2, 1)
        byte1 = pos_to_byte(source_file2, pos1)
        @test byte1 == ncodeunits("function tεst_leΤ_χ()\n ") + 1
        @test is_at(ex2[1], byte1)                              # function
        @test is_at(ex2[1][2], byte1)                           # function body
        @test !is_at(ex2[1][2][1], byte1)                       # (= x 0)
        pos2 = (6, 1)
        byte2 = pos_to_byte(source_file2, pos2)
        @test is_at(ex2, byte2)
        @test is_at(ex2[1][2][2], byte2)                        # (call map ...)
        @test is_at(ex2[1][2][2][3][2], byte2)                  # do block
        @test !is_at(ex2[1][2][2][3][2][1], byte2)              # (op= x + 1)
        @test !is_at(ex2[1][2][2][3][2][2], byte2)              # let
        pos3 = (8, 1)
        byte3 = pos_to_byte(source_file2, pos3)
        @test is_at(ex2[1][2][2][3][2][2], byte3)               # let
        @test !is_at(ex2[1][2][2][3][2][2][2], byte3)           # let body
        @test !is_at(ex2[1][2][2][3][2][2][2][1], byte3)        # (return (-> () x))
        pos4 = (12, 100)
        @test_throws BoundsError pos_to_byte(source_file2, pos4)
        pos5 = (17, 12)
        byte5 = pos_to_byte(source_file2, pos5)
        @test is_at(ex2[2], byte5)                              # comprehension
        @test is_at(ex2[2][1][2][1], byte5)                     # (in f (call tεst_leΤ_χ))
        @test !is_at(ex2[2][1][2][1][2], byte5)                 # (call tεst_leΤ_χ)

        ## src3
        pos1 = (5, 5)
        byte1 = pos_to_byte(source_file3, pos1)
        @test is_at(ex3[2][1][2], byte1)                        # let
        @test is_at(ex3[2][1][2][2], byte1)                     # let body
        @test !is_at(ex3[2][1][1], byte1)                       # (y)
        pos2 = (8, 1)
        byte2 = pos_to_byte(source_file3, pos2)
        @test is_at(ex3[2][2], byte2)                           # map
        @test is_at(ex3[2][2][3], byte2)                        # do
        @test is_at(ex3[2][2][3][2], byte2)                     # do body block
        @test !is_at(ex3[2][2][3][2][1], byte2)                 # do body contents (if)
        pos3 = (15, 23)
        byte3 = pos_to_byte(source_file3, pos3)
        @test is_at(ex3[2][2][3][2][1], byte3)                  # do body contents (if)
        @test is_at(ex3[2][2][3][2][1][3][2], byte3)            # elseif body
        @test !is_at(ex3[2][2][3][2][1][3][2][1], byte3)        # (local (= z 2))
        @test is_at(ex3[2][2][3][2][1][3][2][2][1][3], byte3)   # (y)
        @test !is_at(ex3[2][2][3][2][1][3][2][2][1][2], byte3)  # (+)
        pos4 = (20, 10)
        byte4 = pos_to_byte(source_file3, pos4)
        @test is_at(ex3[2], byte4)                              # function body
    end

    @testset "Find variables" begin
        ## src1
        pos1 = (3, 1)
        vars = find_all_scope_vars(Main, ex1, pos1)
        assignments, locals, destructured_args, globals, used_names, used_bindings =
            collect.(vars)  # Convert sets to vectors.
        @test all(isempty, [locals, destructured_args, globals, used_bindings])
        # There should be only one assignment, `const a = 2`.
        @test length(assignments) == 1
        @test assignments[1].first isa JuliaLowering.NameKey
        @test _name(assignments[1]) == "a"
        @test assignments[1].second isa JuliaLowering.SyntaxTree
        # There should be two used names, `f` and `x`.
        @test length(used_names) == 2
        @test Set([_name(used_names[1]), _name(used_names[2])]) == Set(["f", "x"])

        pos2 = (5, 1)
        vars = find_all_scope_vars(Main, ex1, pos2)
        assignments, locals, destructured_args, globals, used_names, used_bindings =
            collect.(vars)
        @test all(isempty, [locals, destructured_args, used_bindings])
        # There should be a global `a` coming from `global a`.
        @test length(globals) == 1
        @test JuliaLowering.source_location(globals[1].second) == (4, 5)
        @test _name(globals[1]) == "a"
        # There should still be only one assignment, `const a = 2`.
        @test length(assignments) == 1
        @test _name(assignments[1]) == "a"
        # There should still be two used names, `f` and `x`.
        @test length(used_names) == 2
        @test Set([_name(used_names[1]), _name(used_names[2])]) == Set(["f", "x"])

        pos3 = (8, 13)
        vars = find_all_scope_vars(Main, ex1, pos3)
        assignments, locals, destructured_args, globals, used_names, used_bindings =
            collect.(vars)
        @test all(isempty, [locals, destructured_args, used_bindings])
        # The global and the assinment remain.
        @test length(globals) == 1
        @test length(assignments) == 1
        # Now we're using `a` as well.
        @test length(used_names) == 3
        @test Set([_name(used_names[1]), _name(used_names[2]), _name(used_names[3])]) ==
            Set(["f", "x", "a"])

        pos4 = (9, 1)
        vars = find_all_scope_vars(Main, ex1, pos4)
        assignments, locals, destructured_args, globals, used_names, used_bindings =
            collect.(vars)
        @test all(isempty, [locals, destructured_args, used_bindings])
        # The global and the assinment remain.
        @test length(globals) == 1
        @test length(assignments) == 1
        # Now we're using `+` as well and we have 2 `x`s.
        @test length(used_names) == 5
        @test Set([_name(used_names[1]),
                   _name(used_names[2]),
                   _name(used_names[3]),
                   _name(used_names[4]),
                   _name(used_names[5])]) == Set(["f", "x", "a", "+"])

        ## src2
        pos1 = (2, 1)
        vars = find_all_scope_vars(Main, ex2, pos1)
        assignments, locals, destructured_args, globals, used_names, used_bindings =
            collect.(vars)
        # We only have one used name (the function name), everything else is empty.
        @test all(isempty, [assignments, locals, destructured_args, globals, used_bindings])
        @test length(used_names) == 1
        @test _name(used_names[1]) == "tεst_leΤ_χ"

        pos2 = (6, 1)
        vars = find_all_scope_vars(Main, ex2, pos2)
        assignments, locals, destructured_args, globals, used_names, used_bindings =
            collect.(vars)
        @test all(isempty, [locals, destructured_args, globals, used_bindings])
        # Here we should have two assignments to `x`, I think.
        # @test length(assignments) == 2
        # @test name(assignments[1]) == name(assignments[2])
        # `used_names`: `tεst_leΤ_χ`, `map`, `:`, `_`, `+`, `x` (from `x += 1`).
        @test length(used_names) == 6
        @test length(findall(p -> _name(p) == "x", used_names)) == 1

        pos3 = (8, 1)
        vars = find_all_scope_vars(Main, ex2, pos3)
        assignments, locals, destructured_args, globals, used_names, used_bindings =
            collect.(vars)
        @test all(isempty, [destructured_args, globals, used_bindings])
        # Now there's one more assignment in `let`.
        # @test length(assignments) == 3
        # One `x` added by `let`.
        @test length(used_names) == 7
        @test length(findall(p -> _name(p) == "x", used_names)) == 2

        ## src3
        pos1 = (5, 5)
        vars = find_all_scope_vars(Main, ex3, pos1)
        assignments, locals, destructured_args, globals, used_names, used_bindings =
            collect.(vars)
        @test all(isempty, [locals, destructured_args, globals, used_bindings])
        # We should have assignments to `foo`, `y` and `aa`.
        @test length(assignments) == 3
        @test sort(_names(assignments)) == ["aa", "foo", "y"]
        # The names used are `foo`, `aa` and `a` (twice).
        @test length(used_names) == 4
        @test length(findall(p -> _name(p) == "a", used_names)) == 2

        pos2 = (8, 1)
        vars = find_all_scope_vars(Main, ex3, pos2)
        assignments, locals, destructured_args, globals, used_names, used_bindings =
            collect.(vars)
        @test all(isempty, [locals, destructured_args, globals, used_bindings])
        # We don't have the assignment to `aa` anymore.
        @test length(assignments) == 2
        # We have only one use of `a` and no uses of `aa`. We have in addition `A`, `B`,
        # `C`, `map` and `x`.
        @test length(used_names) == 7
        @test length(findall(p -> _name(p) == "a", used_names)) == 1
        @test !("aa" in _names(used_names))
        @test issubset(["map", "A", "B", "C", "x"], _names(used_names))

        pos3 = (15, 23)
        vars = find_all_scope_vars(Main, ex3, pos3)
        assignments, locals, destructured_args, globals, used_names, used_bindings =
            collect.(vars)
        @test all(isempty, [globals, used_bindings])
        # There should be a `locals` entry for `z`.
        @test !isempty(locals)
        @test length(locals) == 1
        @test _name(locals[1]) == "z"
        # Shouldn't there be an entry for `y`?
        # @test !isempty(destructures_args)
        # @test ...
        # There's a new assignment to `z`.
        @test length(assignments) == 3
        @test "z" in _names(assignments)
        # `x` is used 4 times: once at line 7, twice at line 9 (why is there no entry for
        # the `x` in `iseven(x)`?) and once at line 11.
        appearances_x = findall(p -> _name(p) == "x", used_names)
        # @test length(appearances_x) == 4
        # All other names are used only once.
        once = ["map", "<", "iseven", "==", "+", "foo", "a", "A", "B", "C", "z"]
        appearances_others =
            [findall(p -> _name(p) == n, used_names) for n in once]
        @test all(l -> l == 1, length.(appearances_others))
        @test length(used_names) == length(appearances_x) + length(appearances_others)

        pos4 = (20, 10)
        vars = find_all_scope_vars(Main, ex3, pos4)
        assignments, locals, destructured_args, globals, used_names, used_bindings =
            collect.(vars)
        @test all(isempty, [locals, globals, used_bindings])
        # Again, shouldn't there be an entry for `y`?
        # @test !isempty(destructures_args)
        # @test ...
        # There should be the same assignments as for `pos2`.
        # Can this fail because of the elements' order?
        @test length(assignments) == 2
        @test sort(_names(assignments)) == ["foo", "y"]
        # Nothing in the `do` block should be available anymore.
        all_names = ["foo", "a", "map", "A", "B", "C"]
        @test sort(all_names) == sort(_names(used_names))

        ## src4
        pos1 = (3, 1)
        vars = find_all_scope_vars(Main, ex4, pos1)
        assignments, locals, destructured_args, globals, used_names, used_bindings =
            collect.(vars)
        @test all(isempty, [locals, destructured_args, globals, used_bindings])
        # Assignments to `foo` and `b`.
        @test sort(_names(assignments)) == ["b", "foo"]
        # Only `foo`, `a` and `+` are used, `a` twice.
        @test sort(_names(used_names)) == ["+", "a", "a", "foo"]

        pos2 = (7, 1)
        vars = find_all_scope_vars(Main, ex4, pos2)
        assignments, locals, destructured_args, globals, used_names, used_bindings =
            collect.(vars)
        @test all(isempty, [destructured_args, globals, used_bindings])
        # There should be a `locals` entry for `b`.
        @test !isempty(locals)
        @test length(locals) == 1
        @test _name(locals[1]) == "b"
        # There should be six assignments, two for `b`.
        @test sort(_names(assignments)) == ["b", "b", "c", "c2", "d", "foo"]
        # `foo` is used once.
        appearances_foo = findall(n -> _name(n) == "foo", used_names)
        @test length(appearances_foo) == 1
        # `a` is used twice.
        appearances_a = findall(n -> _name(n) == "a", used_names)
        @test length(appearances_a) == 2
        # `+` is used once.
        appearances_plus = findall(n -> _name(n) == "+", used_names)
        @test length(appearances_plus) == 1
        # `b` should be used once (the local `b`). Why is there also a use at line 4?
        # (In `let b = 1`.)
        appearances_b = findall(n -> _name(n) == "b", used_names)
        # @test length(appearances_b) == 1
        # local_b_node = used_names[appearances_b[1]].second
        # local_b_byte = JuliaLowering.sourceref(local_b_node).first_byte
        # @test pos_to_byte(source_file4, (5, 12)) == local_b_byte
        # `c` is used once.
        appearances_c = findall(n -> _name(n) == "c", used_names)
        @test length(appearances_c) == 1
        # `*` is used once.
        appearances_mult = findall(n -> _name(n) == "*", used_names)
        @test length(appearances_mult) == 1
        @test length(used_names) ==
            length(appearances_foo) + length(appearances_plus) + length(appearances_mult) +
            length(appearances_a) + length(appearances_b) + length(appearances_c)

        pos3 = (10, 1)
        vars = find_all_scope_vars(Main, ex4, pos3)
        assignments, locals, destructured_args, globals, used_names, used_bindings =
            collect.(vars)
        # No more locals.
        @test all(isempty, [locals, destructured_args, globals, used_bindings])
        # Only three assignments.
        @test sort(_names(assignments)) == ["b", "d", "foo"]
        # Uses for `foo`, `a` (twice) and `+`.
        @test sort(_names(used_names)) == ["+", "a", "a", "foo"]

        pos4 = (13, 1)
        vars = find_all_scope_vars(Main, ex4, pos4)
        assignments, locals, destructured_args, globals, used_names, used_bindings =
            collect.(vars)
        @test all(isempty, [locals, destructured_args, used_bindings])
        # There should be a `globals` entry for `d`.
        @test !isempty(globals)
        @test length(globals) == 1
        @test _name(globals[1]) == "d"
        global_d_byte = JuliaLowering.sourceref(globals[1].second).first_byte
        @test pos_to_byte(source_file4, (11, 4)) == global_d_byte
        # One more assignment (to `x`).
        @test sort(_names(assignments)) == ["b", "d", "foo", "x"]
        # Extra uses for `b` and `d` (the global one).
        @test sort(_names(used_names)) == ["+", "a", "a", "b", "d", "foo"]
        global_d_use_node = used_names[findall(n -> _name(n) == "d", used_names)[1]].second
        global_d_use_byte = JuliaLowering.sourceref(global_d_use_node).first_byte
        @test pos_to_byte(source_file4, (12, 12)) == global_d_use_byte
    end
end

module call_stack_test
    use erloff_call_stack_m, only: call_stack_t
    use iso_varying_string, only: operator(//)
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: procedure_t
    use veggies, only: &
            Result_t, &
            Test_Item_t, &
            assert_Includes, &
            assert_Not, &
            assert_That, &
            describe, &
            it

    implicit none
    private

    public :: test_call_stack
contains
    function test_call_stack() result(tests)
        type(Test_Item_t) :: tests

        type(Test_Item_t) :: individual_tests(5)

        individual_tests(1) = it( &
                "Can tell if it originated from a module", &
                checkOriginatedFromModule)
        individual_tests(2) = it( &
                "Can tell if it originated from a procedure", &
                checkOriginatedFromProcedure)
        individual_tests(3) = it( &
                "Can tell if it contains a module", &
                checkContainsModule)
        individual_tests(4) = it( &
                "Can tell if it contains a procedure", &
                checkContainsProcedure)
        individual_tests(5) = it( &
                "String includes the given module and procedure names", &
                checkStringIncludes)
        tests = describe("CallStack_t", individual_tests)
    end function test_call_stack

    pure function checkOriginatedFromModule() result(result_)
        type(Result_t) :: result_

        type(Module_t) :: another_module
        type(procedure_t) :: another_procedure
        type(Module_t) :: the_module
        type(procedure_t) :: the_procedure
        type(call_stack_t) :: stack

        another_module = module_t("Another_m")
        another_procedure = procedure_t("another")
        the_module = module_t("Some_m")
        the_procedure = procedure_t("some")
        stack = call_stack_t(the_module, the_procedure)
        stack = stack%with_names_prepended(another_module, another_procedure)

        result_ = &
                assert_That( &
                        stack.originatedFrom.the_module, &
                        stack%repr() // '.originatedFrom.' // the_module%repr()) &
                .and.assert_Not( &
                        stack.originatedFrom.another_module, &
                        stack%repr() // '.originatedFrom.' // another_module%repr())
    end function checkOriginatedFromModule

    pure function checkOriginatedFromProcedure() result(result_)
        type(Result_t) :: result_

        type(Module_t) :: another_module
        type(procedure_t) :: another_procedure
        type(Module_t) :: the_module
        type(procedure_t) :: the_procedure
        type(call_stack_t) :: stack

        another_module = module_t("Another_m")
        another_procedure = procedure_t("another")
        the_module = module_t("Some_m")
        the_procedure = procedure_t("some")
        stack = call_stack_t(the_module, the_procedure)
        stack = stack%with_names_prepended(another_module, another_procedure)

        result_ = &
                assert_That( &
                        stack.originatedFrom.the_procedure, &
                        stack%repr() // '.originatedFrom.' // the_procedure%repr()) &
                .and.assert_Not( &
                        stack.originatedFrom.another_procedure, &
                        stack%repr() // '.originatedFrom.' // another_procedure%repr())
    end function checkOriginatedFromProcedure

    pure function checkContainsModule() result(result_)
        type(Result_t) :: result_

        type(Module_t) :: another_module
        type(procedure_t) :: another_procedure
        type(Module_t) :: other_module
        type(Module_t) :: the_module
        type(procedure_t) :: the_procedure
        type(call_stack_t) :: stack

        another_module = module_t("Another_m")
        another_procedure = procedure_t("another")
        other_module = module_t("Other_m")
        the_module = module_t("Some_m")
        the_procedure = procedure_t("some")
        stack = call_stack_t(the_module, the_procedure)
        stack = stack%with_names_prepended(another_module, another_procedure)

        result_ = &
                assert_That( &
                        stack.includes.the_module, &
                        stack%repr() // '.includes.' // the_module%repr()) &
                .and.assert_Not( &
                        stack.includes.other_module, &
                        stack%repr() // '.includes.' // other_module%repr())
    end function checkContainsModule

    pure function checkContainsProcedure() result(result_)
        type(Result_t) :: result_

        type(Module_t) :: another_module
        type(procedure_t) :: another_procedure
        type(procedure_t) :: other_procedure
        type(Module_t) :: the_module
        type(procedure_t) :: the_procedure
        type(call_stack_t) :: stack

        another_module = module_t("Another_m")
        another_procedure = procedure_t("another")
        other_procedure = procedure_t("other")
        the_module = module_t("Some_m")
        the_procedure = procedure_t("some")
        stack = call_stack_t(the_module, the_procedure)
        stack = stack%with_names_prepended(another_module, another_procedure)

        result_ = &
                assert_That( &
                        stack.includes.the_procedure, &
                        stack%repr() // '.includes.' // the_procedure%repr()) &
                .and.assert_Not( &
                        stack.includes.other_procedure, &
                        stack%repr() // '.includes.' // other_procedure%repr())
    end function checkContainsProcedure

    pure function checkStringIncludes() result(result_)
        type(Result_t) :: result_

        type(Module_t) :: another_module
        type(procedure_t) :: another_procedure
        type(Module_t) :: the_module
        type(procedure_t) :: the_procedure
        type(call_stack_t) :: stack

        another_module = module_t("Another_m")
        another_procedure = procedure_t("another")
        the_module = module_t("Some_m")
        the_procedure = procedure_t("some")
        stack = call_stack_t(the_module, the_procedure)
        stack = stack%with_names_prepended(another_module, another_procedure)

        result_ = &
                assert_Includes(the_module%to_string(), stack%to_string()) &
                .and.assert_Includes(the_procedure%to_string(), stack%to_string()) &
                .and.assert_Includes(another_module%to_string(), stack%to_string()) &
                .and.assert_Includes(another_procedure%to_string(), stack%to_string())
    end function checkStringIncludes
end module call_stack_test

module call_stack_test
    implicit none
    private

    public :: test_call_stack
contains
    function test_call_stack() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(4)

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
        tests = describe("CallStack_t", individual_tests)
    end function test_call_stack

    pure function checkOriginatedFromModule() result(result_)
        use Call_stack_m, only: CallStack_t, CallStack
        use iso_varying_string, only: operator(//)
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(CallStack_t) :: stack

        stack = CallStack("Some_module_m", "someProcedure")
        stack = stack%prependNames("Another_module_m", "anotherProcedure")

        result_ = &
                assertThat( &
                        stack.originatingModuleIs."Some_module_m", &
                        stack%repr() // '.originatingModuleIs."Some_module_m"') &
                .and.assertNot( &
                        stack.originatingModuleIs."Another_module_m", &
                        stack%repr() // '.originatingModuleIs."Another_module_m"')
    end function checkOriginatedFromModule

    pure function checkOriginatedFromProcedure() result(result_)
        use Call_stack_m, only: CallStack_t, CallStack
        use iso_varying_string, only: operator(//)
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(CallStack_t) :: stack

        stack = CallStack("Some_module_m", "someProcedure")
        stack = stack%prependNames("Another_module_m", "anotherProcedure")

        result_ = &
                assertThat( &
                        stack.originatingProcedureIs."someProcedure", &
                        stack%repr() // '.originatingProcedureIs."someProcedure"') &
                .and.assertNot( &
                        stack.originatingProcedureIs."anotherProcedure", &
                        stack%repr() // '.originatingProcedureIs."anotherProcedure"')
    end function checkOriginatedFromProcedure

    pure function checkContainsModule() result(result_)
        use Call_stack_m, only: CallStack_t, CallStack
        use iso_varying_string, only: operator(//)
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(CallStack_t) :: stack

        stack = CallStack("Some_module_m", "someProcedure")
        stack = stack%prependNames("Another_module_m", "anotherProcedure")

        result_ = &
                assertThat( &
                        stack.containsModule."Another_module_m", &
                        stack%repr() // '.containsModule."Another_module_m"') &
                .and.assertNot( &
                        stack.containsModule."Other_module_m", &
                        stack%repr() // '.containsModule."Other_module_m"')
    end function checkContainsModule

    pure function checkContainsProcedure() result(result_)
        use Call_stack_m, only: CallStack_t, CallStack
        use iso_varying_string, only: operator(//)
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(CallStack_t) :: stack

        stack = CallStack("Some_module_m", "someProcedure")
        stack = stack%prependNames("Another_module_m", "anotherProcedure")

        result_ = &
                assertThat( &
                        stack.containsProcedure."anotherProcedure", &
                        stack%repr() // '.containsProcedure."anotherProcedure"') &
                .and.assertNot( &
                        stack.containsProcedure."otherProcedure", &
                        stack%repr() // '.containsProcedure."otherProcedure"')
    end function checkContainsProcedure
end module call_stack_test

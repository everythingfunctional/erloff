module call_stack_entry_test
    implicit none
    private

    public :: test_call_stack_entry
contains
    function test_call_stack_entry() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it( &
                "Can tell if it is from a module", checkIsFromModule)
        individual_tests(2) = it( &
                "Can tell if it is from a procedure", checkIsFromProcedure)
        tests = describe("CallStackEntry_t", individual_tests)
    end function test_call_stack_entry

    pure function checkIsFromModule() result(result_)
        use Call_stack_entry_m, only: CallStackEntry_t, CallStackEntry
        use iso_varying_string, only: operator(//)
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(CallStackEntry_t) :: entry_

        entry_ = CallStackEntry("Some_module_m", "someProcedure")

        result_ = &
                assertThat( &
                        entry_.isFromModule."Some_module_m", &
                        entry_%repr() // '.isFromModule."Some_module_m"') &
                .and.assertNot( &
                        entry_.isFromModule."Other_module_m", &
                        entry_%repr() // '.isFromModule."Other_module_m"')
    end function checkIsFromModule

    pure function checkIsFromProcedure() result(result_)
        use Call_stack_entry_m, only: CallStackEntry_t, CallStackEntry
        use iso_varying_string, only: operator(//)
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(CallStackEntry_t) :: entry_

        entry_ = CallStackEntry("Some_module_m", "someProcedure")

        result_ = &
                assertThat( &
                        entry_.isFromProcedure."someProcedure", &
                        entry_%repr() // '.isFromProcedure."someProcedure"') &
                .and.assertNot( &
                        entry_.isFromProcedure."otherProcedure", &
                        entry_%repr() // '.isFromProcedure."otherProcedure"')
    end function checkIsFromProcedure
end module call_stack_entry_test

module call_stack_entry_test
    implicit none
    private

    public :: test_call_stack_entry
contains
    function test_call_stack_entry() result(tests)
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = It( &
                "Can tell if it is from a module", checkIsFromModule)
        individual_tests(2) = It( &
                "Can tell if it is from a procedure", checkIsFromProcedure)
        tests = Describe("CallStackEntry_t", individual_tests)
    end function test_call_stack_entry

    function checkIsFromModule() result(result_)
        use Call_stack_entry_m, only: CallStackEntry_t, CallStackEntry
        use iso_varying_string, only: operator(//)
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(CallStackEntry_t) :: entry_
        type(Module_t) :: other_module
        type(Module_t) :: the_module
        type(Procedure_t) :: the_procedure

        other_module = Module_("Other_m")
        the_module = Module_("Some_m")
        the_procedure = Procedure_("some")
        entry_ = CallStackEntry(the_module, the_procedure)

        result_ = &
                assertThat( &
                        entry_.isFrom.the_module, &
                        entry_%repr() // '.isFrom.' // the_module%repr()) &
                .and.assertNot( &
                        entry_.isFrom.other_module, &
                        entry_%repr() // '.isFrom.' // other_module%repr())
    end function checkIsFromModule

    function checkIsFromProcedure() result(result_)
        use Call_stack_entry_m, only: CallStackEntry_t, CallStackEntry
        use iso_varying_string, only: operator(//)
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(CallStackEntry_t) :: entry_
        type(Procedure_t) :: other_procedure
        type(Module_t) :: the_module
        type(Procedure_t) :: the_procedure

        other_procedure = Procedure_("other")
        the_module = Module_("Some_m")
        the_procedure = Procedure_("some")

        entry_ = CallStackEntry(the_module, the_procedure)

        result_ = &
                assertThat( &
                        entry_.isFrom.the_procedure, &
                        entry_%repr() // '.isFrom.' // the_procedure%repr()) &
                .and.assertNot( &
                        entry_.isFrom.other_procedure, &
                        entry_%repr() // '.isFrom.' // other_procedure%repr())
    end function checkIsFromProcedure
end module call_stack_entry_test

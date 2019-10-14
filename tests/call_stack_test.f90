module call_stack_test
    implicit none
    private

    public :: test_call_stack
contains
    function test_call_stack() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it( &
                "Can tell if it originated from a module", &
                checkOriginatedFromModule)
        tests = describe("CallStack_t", individual_tests)
    end function test_call_stack

    function checkOriginatedFromModule() result(result_)
        use Call_stack_m, only: CallStack_t, CallStack
        use iso_varying_string, only: operator(//)
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(Module_t) :: another_module
        type(Procedure_t) :: another_procedure
        type(Module_t) :: the_module
        type(Procedure_t) :: the_procedure
        type(CallStack_t) :: stack

        another_module = Module_("Another_m")
        another_procedure = Procedure_("another")
        the_module = Module_("Some_m")
        the_procedure = Procedure_("some")
        stack = CallStack(the_module, the_procedure)
        call stack%prependNames(another_module, another_procedure)

        result_ = &
                assertThat( &
                        stack.originatedFrom.the_module, &
                        stack%repr() // '.originatedFrom.' // the_module%repr()) &
                .and.assertNot( &
                        stack.originatedFrom.another_module, &
                        stack%repr() // '.originatedFrom.' // another_module%repr())
    end function checkOriginatedFromModule
end module call_stack_test

module call_stack_test
    implicit none
    private

    public :: test_call_stack
contains
    function test_call_stack() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(5)

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

    function checkOriginatedFromModule() result(result_)
        use Call_stack_m, only: CallStack_t, CallStack
        use iso_varying_string, only: operator(//)
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(Module_t), pointer :: another_module
        type(Procedure_t), pointer :: another_procedure
        type(Module_t), pointer :: the_module
        type(Procedure_t), pointer :: the_procedure
        type(CallStack_t), pointer :: stack

        another_module => Module_("Another_m")
        another_procedure => Procedure_("another")
        the_module => Module_("Some_m")
        the_procedure => Procedure_("some")
        stack => CallStack(the_module, the_procedure)
        call stack%prependNames(another_module, another_procedure)

        result_ = &
                assertThat( &
                        stack.originatedFrom.the_module, &
                        stack%repr() // '.originatedFrom.' // the_module%repr()) &
                .and.assertNot( &
                        stack.originatedFrom.another_module, &
                        stack%repr() // '.originatedFrom.' // another_module%repr())
        deallocate(stack)
    end function checkOriginatedFromModule

    function checkOriginatedFromProcedure() result(result_)
        use Call_stack_m, only: CallStack_t, CallStack
        use iso_varying_string, only: operator(//)
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(Module_t), pointer :: another_module
        type(Procedure_t), pointer :: another_procedure
        type(Module_t), pointer :: the_module
        type(Procedure_t), pointer :: the_procedure
        type(CallStack_t), pointer :: stack

        another_module => Module_("Another_m")
        another_procedure => Procedure_("another")
        the_module => Module_("Some_m")
        the_procedure => Procedure_("some")
        stack => CallStack(the_module, the_procedure)
        call stack%prependNames(another_module, another_procedure)

        result_ = &
                assertThat( &
                        stack.originatedFrom.the_procedure, &
                        stack%repr() // '.originatedFrom.' // the_procedure%repr()) &
                .and.assertNot( &
                        stack.originatedFrom.another_procedure, &
                        stack%repr() // '.originatedFrom.' // another_procedure%repr())
        deallocate(stack)
    end function checkOriginatedFromProcedure

    function checkContainsModule() result(result_)
        use Call_stack_m, only: CallStack_t, CallStack
        use iso_varying_string, only: operator(//)
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(Module_t), pointer :: another_module
        type(Procedure_t), pointer :: another_procedure
        type(Module_t), pointer :: other_module
        type(Module_t), pointer :: the_module
        type(Procedure_t), pointer :: the_procedure
        type(CallStack_t), pointer :: stack

        another_module => Module_("Another_m")
        another_procedure => Procedure_("another")
        other_module => Module_("Other_m")
        the_module => Module_("Some_m")
        the_procedure => Procedure_("some")
        stack => CallStack(the_module, the_procedure)
        call stack%prependNames(another_module, another_procedure)

        result_ = &
                assertThat( &
                        stack.includes.the_module, &
                        stack%repr() // '.includes.' // the_module%repr()) &
                .and.assertNot( &
                        stack.includes.other_module, &
                        stack%repr() // '.includes.' // other_module%repr())
        deallocate(stack)
        deallocate(other_module)
    end function checkContainsModule

    function checkContainsProcedure() result(result_)
        use Call_stack_m, only: CallStack_t, CallStack
        use iso_varying_string, only: operator(//)
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(Module_t), pointer :: another_module
        type(Procedure_t), pointer :: another_procedure
        type(Procedure_t), pointer :: other_procedure
        type(Module_t), pointer :: the_module
        type(Procedure_t), pointer :: the_procedure
        type(CallStack_t), pointer :: stack

        another_module => Module_("Another_m")
        another_procedure => Procedure_("another")
        other_procedure => Procedure_("other")
        the_module => Module_("Some_m")
        the_procedure => Procedure_("some")
        stack => CallStack(the_module, the_procedure)
        call stack%prependNames(another_module, another_procedure)

        result_ = &
                assertThat( &
                        stack.includes.the_procedure, &
                        stack%repr() // '.includes.' // the_procedure%repr()) &
                .and.assertNot( &
                        stack.includes.other_procedure, &
                        stack%repr() // '.includes.' // other_procedure%repr())
        deallocate(stack)
        deallocate(other_procedure)
    end function checkContainsProcedure

    function checkStringIncludes() result(result_)
        use Call_stack_m, only: CallStack_t, CallStack
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertIncludes

        type(Result_t) :: result_

        type(Module_t), pointer :: another_module
        type(Procedure_t), pointer :: another_procedure
        type(Module_t), pointer :: the_module
        type(Procedure_t), pointer :: the_procedure
        type(CallStack_t), pointer :: stack

        another_module => Module_("Another_m")
        another_procedure => Procedure_("another")
        the_module => Module_("Some_m")
        the_procedure => Procedure_("some")
        stack => CallStack(the_module, the_procedure)
        call stack%prependNames(another_module, another_procedure)

        result_ = &
                assertIncludes(the_module%toString(), stack%toString()) &
                .and.assertIncludes(the_procedure%toString(), stack%toString()) &
                .and.assertIncludes(another_module%toString(), stack%toString()) &
                .and.assertIncludes(another_procedure%toString(), stack%toString())
        deallocate(stack)
    end function checkStringIncludes
end module call_stack_test

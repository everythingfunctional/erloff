module call_stack_entry_test
    use Call_stack_entry_m, only: CallStackEntry_t, CallStackEntry
    use iso_varying_string, only: operator(//)
    use erloff_module_m, only: module_t
    use Procedure_m, only: Procedure_t, Procedure_
    use vegetables, only: &
            Result_t, &
            Test_Item_t, &
            assert_Includes, &
            assert_Not, &
            assert_That, &
            Describe, &
            It

    implicit none
    private

    public :: test_call_stack_entry
contains
    function test_call_stack_entry() result(tests)
        type(Test_Item_t) :: tests

        type(Test_Item_t) :: individual_tests(3)

        individual_tests(1) = It( &
                "Can tell if it is from a module", checkIsFromModule)
        individual_tests(2) = It( &
                "Can tell if it is from a procedure", checkIsFromProcedure)
        individual_tests(3) = It( &
                "String includes the given module and procedure names", &
                checkStringIncludes)
        tests = Describe("CallStackEntry_t", individual_tests)
    end function test_call_stack_entry

    pure function checkIsFromModule() result(result_)
        type(Result_t) :: result_

        type(CallStackEntry_t) :: entry_
        type(Module_t) :: other_module
        type(Module_t) :: the_module
        type(Procedure_t) :: the_procedure

        other_module = module_t("Other_m")
        the_module = module_t("Some_m")
        the_procedure = Procedure_("some")
        entry_ = CallStackEntry(the_module, the_procedure)

        result_ = &
                assert_That( &
                        entry_.isFrom.the_module, &
                        entry_%repr() // '.isFrom.' // the_module%repr()) &
                .and.assert_Not( &
                        entry_.isFrom.other_module, &
                        entry_%repr() // '.isFrom.' // other_module%repr())
    end function checkIsFromModule

    pure function checkIsFromProcedure() result(result_)
        type(Result_t) :: result_

        type(CallStackEntry_t) :: entry_
        type(Procedure_t) :: other_procedure
        type(Module_t) :: the_module
        type(Procedure_t) :: the_procedure

        other_procedure = Procedure_("other")
        the_module = module_t("Some_m")
        the_procedure = Procedure_("some")

        entry_ = CallStackEntry(the_module, the_procedure)

        result_ = &
                assert_That( &
                        entry_.isFrom.the_procedure, &
                        entry_%repr() // '.isFrom.' // the_procedure%repr()) &
                .and.assert_Not( &
                        entry_.isFrom.other_procedure, &
                        entry_%repr() // '.isFrom.' // other_procedure%repr())
    end function checkIsFromProcedure

    pure function checkStringIncludes() result(result_)
        type(Result_t) :: result_

        type(CallStackEntry_t) :: entry_
        type(Module_t) :: the_module
        type(Procedure_t) :: the_procedure

        the_module = module_t("Some_m")
        the_procedure = Procedure_("some")
        entry_ = CallStackEntry(the_module, the_procedure)

        result_ = &
                assert_Includes(the_module%to_string(), entry_%toString()) &
                .and.assert_Includes(the_procedure%toString(), entry_%toString())
    end function checkStringIncludes
end module call_stack_entry_test

module message_test
    implicit none
    private

    public :: test_message
contains
    function test_message() result(tests)
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = It( &
                "Can tell whether it is of a given type", checkType)
        tests = Describe("Message_t", individual_tests)
    end function test_message

    function checkType() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: &
                Message_t, &
                Debug, &
                Info, &
                DEBUG_TYPE, &
                ERROR_TYPE, &
                GENERAL, &
                INFO_TYPE, &
                INPUTS_TYPE, &
                UNEQUAL_ARRAY_SIZES_TYPE
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        class(Message_t), allocatable :: debug_message
        class(Message_t), allocatable :: info_message

        allocate(debug_message, source = Debug( &
                INPUTS_TYPE, &
                Module_("Some_m"), &
                Procedure_("some"), &
                GENERAL, &
                "Test Message"))

        allocate(info_message, source = Info( &
                UNEQUAL_ARRAY_SIZES_TYPE, &
                Module_("Some_m"), &
                Procedure_("some"), &
                "Test Message"))

        result_ = &
                assertThat( &
                        debug_message.isType.DEBUG_TYPE, &
                        debug_message%repr() // ".isType." // DEBUG_TYPE%repr()) &
                .and.assertThat( &
                        debug_message.isType.INPUTS_TYPE, &
                        debug_message%repr() // ".isType." // INPUTS_TYPE%repr()) &
                .and.assertNot( &
                        debug_message.isType.INFO_TYPE, &
                        debug_message%repr() // ".isType." // INFO_TYPE%repr()) &
                .and.assertNot( &
                        debug_message.isType.ERROR_TYPE, &
                        debug_message%repr() // ".isType." // ERROR_TYPE%repr())
    end function checkType
end module message_test

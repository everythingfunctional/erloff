module message_test
    implicit none
    private

    public :: test_message
contains
    function test_message() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it( &
                "Can tell whether it is of a given type", checkType)
        tests = describe("Message_t", individual_tests)
    end function test_message

    pure function checkType() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: &
                Message_t, &
                Debug, &
                Info, &
                Warning, &
                Fatal, &
                Internal, &
                DEBUG_TYPE, &
                ERROR_TYPE, &
                FATAL_TYPE, &
                GENERAL, &
                INFO_TYPE, &
                INPUTS_TYPE, &
                INTERNAL_TYPE, &
                OUTSIDE_NORMAL_RANGE_TYPE, &
                UNEQUAL_ARRAY_SIZES_TYPE, &
                UNKNOWN_TYPE_TYPE, &
                WARNING_TYPE
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        class(Message_t), allocatable :: debug_message
        class(Message_t), allocatable :: info_message
        class(Message_t), allocatable :: warning_message
        class(Message_t), allocatable :: fatal_message
        class(Message_t), allocatable :: internal_message

        allocate(debug_message, source = Debug( &
                INPUTS_TYPE, &
                "Some_module_m", &
                "someProcedure", &
                GENERAL, &
                "Test Message"))

        allocate(info_message, source = Info( &
                UNEQUAL_ARRAY_SIZES_TYPE, &
                "Some_module_m", &
                "someProcedure", &
                "Test Message"))

        allocate(warning_message, source = Warning( &
                OUTSIDE_NORMAL_RANGE_TYPE, &
                "Some_module_m", &
                "someProcedure", &
                "Test Message"))

        allocate(fatal_message, source = Fatal( &
                UNEQUAL_ARRAY_SIZES_TYPE, &
                "Some_module_m", &
                "someProcedure", &
                "Test Message"))

        allocate(internal_message, source = Internal( &
                UNKNOWN_TYPE_TYPE, &
                "Some_module_m", &
                "someProcedure", &
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
                        debug_message%repr() // ".isType." // ERROR_TYPE%repr()) &
                .and.assertThat( &
                        info_message.isType.INFO_TYPE, &
                        info_message%repr() // ".isType." // INFO_TYPE%repr()) &
                .and.assertThat( &
                        info_message.isType.UNEQUAL_ARRAY_SIZES_TYPE, &
                        info_message%repr() // ".isType." // UNEQUAL_ARRAY_SIZES_TYPE%repr()) &
                .and.assertNot( &
                        info_message.isType.DEBUG_TYPE, &
                        info_message%repr() // ".isType." // DEBUG_TYPE%repr()) &
                .and.assertNot( &
                        info_message.isType.ERROR_TYPE, &
                        info_message%repr() // ".isType." // ERROR_TYPE%repr()) &
                .and.assertThat( &
                        warning_message.isType.WARNING_TYPE, &
                        warning_message%repr() // ".isType." // WARNING_TYPE%repr()) &
                .and.assertThat( &
                        warning_message.isType.OUTSIDE_NORMAL_RANGE_TYPE, &
                        warning_message%repr() // ".isType." // OUTSIDE_NORMAL_RANGE_TYPE%repr()) &
                .and.assertNot( &
                        warning_message.isType.INFO_TYPE, &
                        warning_message%repr() // ".isType." // INFO_TYPE%repr()) &
                .and.assertNot( &
                        warning_message.isType.ERROR_TYPE, &
                        warning_message%repr() // ".isType." // ERROR_TYPE%repr()) &
                .and.assertThat( &
                        fatal_message.isType.ERROR_TYPE, &
                        fatal_message%repr() // ".isType." // ERROR_TYPE%repr()) &
                .and.assertThat( &
                        fatal_message.isType.FATAL_TYPE, &
                        fatal_message%repr() // ".isType." // FATAL_TYPE%repr()) &
                .and.assertThat( &
                        fatal_message.isType.UNEQUAL_ARRAY_SIZES_TYPE, &
                        fatal_message%repr() // ".isType." // UNEQUAL_ARRAY_SIZES_TYPE%repr()) &
                .and.assertNot( &
                        fatal_message.isType.INTERNAL_TYPE, &
                        fatal_message%repr() // ".isType." // INTERNAL_TYPE%repr()) &
                .and.assertThat( &
                        internal_message.isType.ERROR_TYPE, &
                        internal_message%repr() // ".isType." // ERROR_TYPE%repr()) &
                .and.assertThat( &
                        internal_message.isType.INTERNAL_TYPE, &
                        internal_message%repr() // ".isType." // INTERNAL_TYPE%repr()) &
                .and.assertThat( &
                        internal_message.isType.UNKNOWN_TYPE_TYPE, &
                        internal_message%repr() // ".isType." // UNKNOWN_TYPE_TYPE%repr())
    end function checkType
end module message_test

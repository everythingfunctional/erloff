module message_test
    implicit none
    private

    public :: test_message
contains
    function test_message() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(9)

        individual_tests(1) = it( &
                "Can tell whether it is of a given type", checkType)
        individual_tests(2) = it( &
                "Can prepend names to a message", checkPrependNames)
        individual_tests(3) = it( &
                "can tell which procedure it originated from", &
                checkOriginatingProcedure)
        individual_tests(4) = it( &
                "can tell which module it originated from", &
                checkOriginatingModule)
        individual_tests(5) = it( &
                "can tell which procedures it came from", &
                checkFromProcedure)
        individual_tests(6) = it( &
                "can tell which modules it came from", &
                checkFromModule)
        individual_tests(7) = it( &
                "can tell which procedures it came through", &
                checkThroughProcedure)
        individual_tests(8) = it( &
                "can tell which modules it came through", &
                checkThroughModule)
        individual_tests(9) = it( &
                "can tell lots about its contents", &
                checkContents)
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

    pure function checkPrependNames() result(result_)
        use Message_m, only: Message_t, Info
        use strff, only: NEWLINE
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        class(Message_t), allocatable :: message
        class(Message_t), allocatable :: prepended
        character(len=*), parameter :: EXPECTED_MESSAGE = &
                "Another_module_m.anotherProcedure->Some_module_m.someProcedure:" &
                // NEWLINE // "    IN: Test Message"

        allocate(message, source = Info( &
                "Some_module_m", "someProcedure", "Test Message"))
        allocate(prepended, source = message%prependNames( &
                "Another_module_m", "anotherProcedure"))

        result_ = assertEquals(EXPECTED_MESSAGE, prepended%toString())
    end function checkPrependNames

    pure function checkOriginatingProcedure() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Message_t, Info
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        class(Message_t), allocatable :: message
        class(Message_t), allocatable :: prepended

        allocate(message, source = Info( &
                "Some_module_m", "someProcedure", "Test Message"))
        allocate(prepended, source = message%prependNames( &
                "Another_module_m", "anotherProcedure"))

        result_ = &
                assertThat( &
                        prepended.originatedFromProcedure."someProcedure", &
                        prepended%repr() // '.originatedFromProcedure."someProcedure"') &
                .and.assertNot( &
                        prepended.originatedFromProcedure."anotherProcedure", &
                        prepended%repr() // '.originatedFromProcedure."anotherProcedure"') &
                .and.assertNot( &
                        prepended.originatedFromProcedure."yetAnotherProcedure", &
                        prepended%repr() // '.originatedFromProcedure."yetAnotherProcedure"')
    end function checkOriginatingProcedure

    pure function checkOriginatingModule() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Message_t, Info
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        class(Message_t), allocatable :: message
        class(Message_t), allocatable :: prepended

        allocate(message, source = Info( &
                "Some_module_m", "someProcedure", "Test Message"))
        allocate(prepended, source = message%prependNames( &
                "Another_module_m", "anotherProcedure"))

        result_ = &
                assertThat( &
                        prepended.originatedFromModule."Some_module_m", &
                        prepended%repr() // '.originatedFromModule."Some_module_m"') &
                .and.assertNot( &
                        prepended.originatedFromModule."Another_module_m", &
                        prepended%repr() // '.originatedFromModule."Another_module_m"') &
                .and.assertNot( &
                        prepended.originatedFromModule."Yet_another_module_m", &
                        prepended%repr() // '.originatedFromModule."Yet_another_module_m"')
    end function checkOriginatingModule

    pure function checkFromProcedure() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Message_t, Info
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        class(Message_t), allocatable :: message
        class(Message_t), allocatable :: prepended

        allocate(message, source = Info( &
                "Some_module_m", "someProcedure", "Test Message"))
        allocate(prepended, source = message%prependNames( &
                "Another_module_m", "anotherProcedure"))

        result_ = &
                assertThat( &
                        prepended.isFromProcedure."someProcedure", &
                        prepended%repr() // '.isFromProcedure."someProcedure"') &
                .and.assertThat( &
                        prepended.isFromProcedure."anotherProcedure", &
                        prepended%repr() // '.isFromProcedure."anotherProcedure"') &
                .and.assertNot( &
                        prepended.isFromProcedure."yetAnotherProcedure", &
                        prepended%repr() // '.isFromProcedure."yetAnotherProcedure"')
    end function checkFromProcedure

    pure function checkFromModule() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Message_t, Info
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        class(Message_t), allocatable :: message
        class(Message_t), allocatable :: prepended

        allocate(message, source = Info( &
                "Some_module_m", "someProcedure", "Test Message"))
        allocate(prepended, source = message%prependNames( &
                "Another_module_m", "anotherProcedure"))

        result_ = &
                assertThat( &
                        prepended.isFromModule."Some_module_m", &
                        prepended%repr() // '.isFromModule."Some_module_m"') &
                .and.assertThat( &
                        prepended.isFromModule."Another_module_m", &
                        prepended%repr() // '.isFromModule."Another_module_m"') &
                .and.assertNot( &
                        prepended.isFromModule."Yet_another_module_m", &
                        prepended%repr() // '.isFromModule."Yet_another_module_m"')
    end function checkFromModule

    pure function checkThroughProcedure() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Message_t, Info
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        class(Message_t), allocatable :: message
        class(Message_t), allocatable :: prepended

        allocate(message, source = Info( &
                "Some_module_m", "someProcedure", "Test Message"))
        allocate(prepended, source = message%prependNames( &
                "Another_module_m", "anotherProcedure"))

        result_ = &
                assertNot( &
                        prepended.cameThroughProcedure."someProcedure", &
                        prepended%repr() // '.cameThroughProcedure."someProcedure"') &
                .and.assertThat( &
                        prepended.cameThroughProcedure."anotherProcedure", &
                        prepended%repr() // '.cameThroughProcedure."anotherProcedure"') &
                .and.assertNot( &
                        prepended.cameThroughProcedure."yetAnotherProcedure", &
                        prepended%repr() // '.cameThroughProcedure."yetAnotherProcedure"')
    end function checkThroughProcedure

    pure function checkThroughModule() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Message_t, Info
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        class(Message_t), allocatable :: message
        class(Message_t), allocatable :: prepended

        allocate(message, source = Info( &
                "Some_module_m", "someProcedure", "Test Message"))
        allocate(prepended, source = message%prependNames( &
                "Another_module_m", "anotherProcedure"))

        result_ = &
                assertNot( &
                        prepended.cameThroughModule."Some_module_m", &
                        prepended%repr() // '.cameThroughModule."Some_module_m"') &
                .and.assertThat( &
                        prepended.cameThroughModule."Another_module_m", &
                        prepended%repr() // '.cameThroughModule."Another_module_m"') &
                .and.assertNot( &
                        prepended.cameThroughModule."Yet_another_module_m", &
                        prepended%repr() // '.cameThroughModule."Yet_another_module_m"')
    end function checkThroughModule

    pure function checkContents() result(result_)
        use iso_varying_string, only: operator(//), var_str
        use Message_m, only: Message_t, Info
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        class(Message_t), allocatable :: message

        allocate(message, source = Info( &
                "Some_module_m", "someProcedure", "Test Message Content"))

        result_ = &
                assertThat( &
                        message.includes."Test", &
                        message%repr() // '.includes."Test"') &
                .and.assertNot( &
                        message.includes."test", &
                        message%repr() // '.includes."test"') &
                .and.assertThat( &
                        message.includesAnyOf.[var_str("Test"), var_str("else")], &
                        message%repr() // '.includesAnyOf.[var_str("Test"), var_str("else")]') &
                .and.assertThat( &
                        message.includesAnyOf.[var_str("Test"), var_str("Content")], &
                        message%repr() // '.includesAnyOf.[var_str("Test"), var_str("Content")]') &
                .and.assertNot( &
                        message.includesAnyOf.[var_str("test"), var_str("else")], &
                        message%repr() // '.includesAnyOf.[var_str("test"), var_str("else")]') &
                .and.assertThat( &
                        message.includesAllOf.[var_str("Test"), var_str("Message")], &
                        message%repr() // '.includesAllOf.[var_str("Test"), var_str("Message")]') &
                .and.assertNot( &
                        message.includesAllOf.[var_str("test"), var_str("Message")], &
                        message%repr() // '.includesAllOf.[var_str("test"), var_str("Message")]') &
                .and.assertNot( &
                        message.includesAllOf.[var_str("Test"), var_str("message")], &
                        message%repr() // '.includesAllOf.[var_str("Test"), var_str("message")]')
    end function checkContents
end module message_test
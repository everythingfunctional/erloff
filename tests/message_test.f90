module message_test
    use iso_varying_string, only: VARYING_STRING, operator(//), var_str
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
    use Module_m, only: Module_t, Module_
    use Procedure_m, only: Procedure_t, Procedure_
    use Vegetables_m, only: &
            Result_t, &
            TestItem_t, &
            Describe, &
            assertNot, &
            assertThat, &
            It

    implicit none
    private

    public :: test_message
contains
    function test_message() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(8)

        individual_tests(1) = It( &
                "Can tell whether it is of a given type", checkType)
        individual_tests(2) = It( &
                "Can tell which module it originated from", &
                checkOriginatingModule)
        individual_tests(3) = It( &
                "Can tell which procedure it originated from", &
                checkOriginatingProcedure)
        individual_tests(4) = It( &
                "Can tell which modules it came from", &
                checkFromModule)
        individual_tests(5) = It( &
                "Can tell which procedures it came from", &
                checkFromProcedure)
        individual_tests(6) = It( &
                "Can tell which modules it came through", &
                checkThroughModule)
        individual_tests(7) = It( &
                "Can tell which procedures it came through", &
                checkThroughProcedure)
        individual_tests(8) = It( &
                "Can tell lots about its contents", &
                checkContents)
        tests = Describe("Message_t", individual_tests)
    end function test_message

    pure function checkType() result(result_)
        type(Result_t) :: result_

        class(Message_t), allocatable :: debug_message
        class(Message_t), allocatable :: info_message
        class(Message_t), allocatable :: warning_message
        class(Message_t), allocatable :: fatal_message
        class(Message_t), allocatable :: internal_message

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

        allocate(warning_message, source = Warning( &
                OUTSIDE_NORMAL_RANGE_TYPE, &
                Module_("Some_m"), &
                Procedure_("some"), &
                "Test Message"))

        allocate(fatal_message, source = Fatal( &
                UNEQUAL_ARRAY_SIZES_TYPE, &
                Module_("Some_m"), &
                Procedure_("some"), &
                "Test Message"))

        allocate(internal_message, source = Internal( &
                UNKNOWN_TYPE_TYPE, &
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
                        debug_message%repr() // ".isType." // ERROR_TYPE%repr()) &
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

    pure function checkOriginatingModule() result(result_)
        type(Result_t) :: result_

        type(Module_t) :: another_module
        type(Procedure_t) :: another_procedure
        class(Message_t), allocatable :: message
        type(Module_t) :: other_module
        type(Module_t) :: the_module
        type(Procedure_t) :: the_procedure

        the_module = Module_("Some_m")
        the_procedure = Procedure_("some")
        another_module = Module_("Another_m")
        another_procedure = Procedure_("another")
        other_module = Module_("Other_m")
        allocate(message, source = Info( &
                the_module, the_procedure, "Test Message"))
        call message%prependNames(another_module, another_procedure)

        result_ = &
                assertThat( &
                        message.originatedFrom.the_module, &
                        message%repr() // '.originatedFrom.' // the_module%repr()) &
                .and.assertNot( &
                        message.originatedFrom.another_module, &
                        message%repr() // '.originatedFrom.' // another_module%repr()) &
                .and.assertNot( &
                        message.originatedFrom.other_module, &
                        message%repr() // '.originatedFrom.' // other_module%repr())
    end function checkOriginatingModule

    pure function checkOriginatingProcedure() result(result_)
        type(Result_t) :: result_

        type(Module_t) :: another_module
        type(Procedure_t) :: another_procedure
        class(Message_t), allocatable :: message
        type(Procedure_t) :: other_procedure
        type(Module_t) :: the_module
        type(Procedure_t) :: the_procedure

        the_module = Module_("Some_m")
        the_procedure = Procedure_("some")
        another_module = Module_("Another_m")
        another_procedure = Procedure_("another")
        other_procedure = Procedure_("other")
        allocate(message, source = Info( &
                the_module, the_procedure, "Test Message"))
        call message%prependNames(another_module, another_procedure)

        result_ = &
                assertThat( &
                        message.originatedFrom.the_procedure, &
                        message%repr() // '.originatedFrom.' // the_procedure%repr()) &
                .and.assertNot( &
                        message.originatedFrom.another_procedure, &
                        message%repr() // '.originatedFrom.' // another_procedure%repr()) &
                .and.assertNot( &
                        message.originatedFrom.other_procedure, &
                        message%repr() // '.originatedFrom.' // other_procedure%repr())
    end function checkOriginatingProcedure

    pure function checkFromModule() result(result_)
        type(Result_t) :: result_

        type(Module_t) :: another_module
        type(Procedure_t) :: another_procedure
        class(Message_t), allocatable :: message
        type(Module_t) :: other_module
        type(Module_t) :: the_module
        type(Procedure_t) :: the_procedure

        the_module = Module_("Some_m")
        the_procedure = Procedure_("some")
        another_module = Module_("Another_m")
        another_procedure = Procedure_("another")
        other_module = Module_("Other_m")
        allocate(message, source = Info( &
                the_module, the_procedure, "Test Message"))
        call message%prependNames(another_module, another_procedure)

        result_ = &
                assertThat( &
                        message.isFrom.the_module, &
                        message%repr() // '.isFrom.' // the_module%repr()) &
                .and.assertThat( &
                        message.isFrom.another_module, &
                        message%repr() // '.isFrom.' // another_module%repr()) &
                .and.assertNot( &
                        message.isFrom.other_module, &
                        message%repr() // '.isFrom.' // other_module%repr())
    end function checkFromModule

    pure function checkFromProcedure() result(result_)
        type(Result_t) :: result_

        type(Module_t) :: another_module
        type(Procedure_t) :: another_procedure
        class(Message_t), allocatable :: message
        type(Procedure_t) :: other_procedure
        type(Module_t) :: the_module
        type(Procedure_t) :: the_procedure

        the_module = Module_("Some_m")
        the_procedure = Procedure_("some")
        another_module = Module_("Another_m")
        another_procedure = Procedure_("another")
        other_procedure = Procedure_("other")
        allocate(message, source = Info( &
                the_module, the_procedure, "Test Message"))
        call message%prependNames(another_module, another_procedure)

        result_ = &
                assertThat( &
                        message.isFrom.the_procedure, &
                        message%repr() // '.isFrom.' // the_procedure%repr()) &
                .and.assertThat( &
                        message.isFrom.another_procedure, &
                        message%repr() // '.isFrom.' // another_procedure%repr()) &
                .and.assertNot( &
                        message.isFrom.other_procedure, &
                        message%repr() // '.isFrom.' // other_procedure%repr())
    end function checkFromProcedure

    pure function checkThroughModule() result(result_)
        type(Result_t) :: result_

        type(Module_t) :: another_module
        type(Procedure_t) :: another_procedure
        class(Message_t), allocatable :: message
        type(Module_t) :: other_module
        type(Module_t) :: the_module
        type(Procedure_t) :: the_procedure

        the_module = Module_("Some_m")
        the_procedure = Procedure_("some")
        another_module = Module_("Another_m")
        another_procedure = Procedure_("another")
        other_module = Module_("Other_m")
        allocate(message, source = Info( &
                the_module, the_procedure, "Test Message"))
        call message%prependNames(another_module, another_procedure)

        result_ = &
                assertNot( &
                        message.cameThrough.the_module, &
                        message%repr() // '.cameThrough.' // the_module%repr()) &
                .and.assertThat( &
                        message.cameThrough.another_module, &
                        message%repr() // '.cameThrough.' // another_module%repr()) &
                .and.assertNot( &
                        message.cameThrough.other_module, &
                        message%repr() // '.cameThrough.' // other_module%repr())
    end function checkThroughModule

    pure function checkThroughProcedure() result(result_)
        type(Result_t) :: result_

        type(Module_t) :: another_module
        type(Procedure_t) :: another_procedure
        class(Message_t), allocatable :: message
        type(Procedure_t) :: other_procedure
        type(Module_t) :: the_module
        type(Procedure_t) :: the_procedure

        the_module = Module_("Some_m")
        the_procedure = Procedure_("some")
        another_module = Module_("Another_m")
        another_procedure = Procedure_("another")
        other_procedure = Procedure_("other")
        allocate(message, source = Info( &
                the_module, the_procedure, "Test Message"))
        call message%prependNames(another_module, another_procedure)

        result_ = &
                assertNot( &
                        message.cameThrough.the_procedure, &
                        message%repr() // '.cameThrough.' // the_procedure%repr()) &
                .and.assertThat( &
                        message.cameThrough.another_procedure, &
                        message%repr() // '.cameThrough.' // another_procedure%repr()) &
                .and.assertNot( &
                        message.cameThrough.other_procedure, &
                        message%repr() // '.cameThrough.' // other_procedure%repr())
    end function checkThroughProcedure

    pure function checkContents() result(result_)
        type(Result_t) :: result_

        class(Message_t), allocatable :: message
        type(VARYING_STRING) :: includesAnyStrings1(2)
        type(VARYING_STRING) :: includesAnyStrings2(2)
        type(VARYING_STRING) :: includesAnyStrings3(2)
        type(VARYING_STRING) :: includesAllStrings1(2)
        type(VARYING_STRING) :: includesAllStrings2(2)
        type(VARYING_STRING) :: includesAllStrings3(2)

        allocate(message, source = Info( &
                Module_("Some_m"), Procedure_("some"), "Test Message Content"))
        includesAnyStrings1(1) = var_str("Test")
        includesAnyStrings1(2) = var_str("else")
        includesAnyStrings2(1) = var_str("Test")
        includesAnyStrings2(2) = var_str("Content")
        includesAnyStrings3(1) = var_str("test")
        includesAnyStrings3(2) = var_str("else")
        includesAllStrings1(1) = var_str("Test")
        includesAllStrings1(2) = var_str("Message")
        includesAllStrings2(1) = var_str("test")
        includesAllStrings2(2) = var_str("Message")
        includesAllStrings3(1) = var_str("Test")
        includesAllStrings3(2) = var_str("message")

        result_ = &
                assertThat( &
                        message.includes."Test", &
                        message%repr() // '.includes."Test"') &
                .and.assertNot( &
                        message.includes."test", &
                        message%repr() // '.includes."test"') &
                .and.assertThat( &
                        message.includesAnyOf.includesAnyStrings1, &
                        message%repr() // '.includesAnyOf.[var_str("Test"), var_str("else")]') &
                .and.assertThat( &
                        message.includesAnyOf.includesAnyStrings2, &
                        message%repr() // '.includesAnyOf.[var_str("Test"), var_str("Content")]') &
                .and.assertNot( &
                        message.includesAnyOf.includesAnyStrings3, &
                        message%repr() // '.includesAnyOf.[var_str("test"), var_str("else")]') &
                .and.assertThat( &
                        message.includesAllOf.includesAllStrings1, &
                        message%repr() // '.includesAllOf.[var_str("Test"), var_str("Message")]') &
                .and.assertNot( &
                        message.includesAllOf.includesAllStrings2, &
                        message%repr() // '.includesAllOf.[var_str("test"), var_str("Message")]') &
                .and.assertNot( &
                        message.includesAllOf.includesAllStrings3, &
                        message%repr() // '.includesAllOf.[var_str("Test"), var_str("message")]')
    end function checkContents
end module message_test

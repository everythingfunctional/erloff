module message_test
    implicit none
    private

    public :: test_message
contains
    function test_message() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(8)

        individual_tests(1) = it( &
                "Can tell whether it is of a given type", checkType)
        individual_tests(2) = it( &
                "can tell which module it originated from", &
                checkOriginatingModule)
        individual_tests(3) = it( &
                "can tell which procedure it originated from", &
                checkOriginatingProcedure)
        individual_tests(4) = it( &
                "can tell which modules it came from", &
                checkFromModule)
        individual_tests(5) = it( &
                "can tell which procedures it came from", &
                checkFromProcedure)
        individual_tests(6) = it( &
                "can tell which modules it came through", &
                checkThroughModule)
        individual_tests(7) = it( &
                "can tell which procedures it came through", &
                checkThroughProcedure)
        individual_tests(8) = it( &
                "can tell lots about its contents", &
                checkContents)
        tests = describe("Message_t", individual_tests)
    end function test_message

    function checkType() result(result_)
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
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        class(Message_t), pointer :: debug_message
        class(Message_t), pointer :: info_message
        class(Message_t), pointer :: warning_message
        class(Message_t), pointer :: fatal_message
        class(Message_t), pointer :: internal_message

        debug_message => Debug( &
                INPUTS_TYPE, &
                Module_("Some_m"), &
                Procedure_("some"), &
                GENERAL, &
                "Test Message")

        info_message => Info( &
                UNEQUAL_ARRAY_SIZES_TYPE, &
                Module_("Some_m"), &
                Procedure_("some"), &
                "Test Message")

        warning_message => Warning( &
                OUTSIDE_NORMAL_RANGE_TYPE, &
                Module_("Some_m"), &
                Procedure_("some"), &
                "Test Message")

        fatal_message => Fatal( &
                UNEQUAL_ARRAY_SIZES_TYPE, &
                Module_("Some_m"), &
                Procedure_("some"), &
                "Test Message")

        internal_message => Internal( &
                UNKNOWN_TYPE_TYPE, &
                Module_("Some_m"), &
                Procedure_("some"), &
                "Test Message")

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
        deallocate(debug_message)
        deallocate(info_message)
        deallocate(warning_message)
        deallocate(fatal_message)
        deallocate(internal_message)
    end function checkType

    function checkOriginatingModule() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Message_t, Info
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(Module_t), pointer :: another_module
        type(Procedure_t), pointer :: another_procedure
        class(Message_t), pointer :: message
        type(Module_t), pointer :: other_module
        type(Module_t), pointer :: the_module
        type(Procedure_t), pointer :: the_procedure

        the_module => Module_("Some_m")
        the_procedure => Procedure_("some")
        another_module => Module_("Another_m")
        another_procedure => Procedure_("another")
        other_module => Module_("Other_m")
        message => Info( &
                the_module, the_procedure, "Test Message")
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
        deallocate(message)
        deallocate(other_module)
    end function checkOriginatingModule

    function checkOriginatingProcedure() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Message_t, Info
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(Module_t), pointer :: another_module
        type(Procedure_t), pointer :: another_procedure
        class(Message_t), pointer :: message
        type(Procedure_t), pointer :: other_procedure
        type(Module_t), pointer :: the_module
        type(Procedure_t), pointer :: the_procedure

        the_module => Module_("Some_m")
        the_procedure => Procedure_("some")
        another_module => Module_("Another_m")
        another_procedure => Procedure_("another")
        other_procedure => Procedure_("other")
        message => Info( &
                the_module, the_procedure, "Test Message")
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
        deallocate(message)
        deallocate(other_procedure)
    end function checkOriginatingProcedure

    function checkFromModule() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Message_t, Info
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(Module_t), pointer :: another_module
        type(Procedure_t), pointer :: another_procedure
        class(Message_t), pointer :: message
        type(Module_t), pointer :: other_module
        type(Module_t), pointer :: the_module
        type(Procedure_t), pointer :: the_procedure

        the_module => Module_("Some_m")
        the_procedure => Procedure_("some")
        another_module => Module_("Another_m")
        another_procedure => Procedure_("another")
        other_module => Module_("Other_m")
        message => Info( &
                the_module, the_procedure, "Test Message")
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
        deallocate(message)
        deallocate(other_module)
    end function checkFromModule

    function checkFromProcedure() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Message_t, Info
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(Module_t), pointer :: another_module
        type(Procedure_t), pointer :: another_procedure
        class(Message_t), pointer :: message
        type(Procedure_t), pointer :: other_procedure
        type(Module_t), pointer :: the_module
        type(Procedure_t), pointer :: the_procedure

        the_module => Module_("Some_m")
        the_procedure => Procedure_("some")
        another_module => Module_("Another_m")
        another_procedure => Procedure_("another")
        other_procedure => Procedure_("other")
        message => Info( &
                the_module, the_procedure, "Test Message")
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
        deallocate(message)
        deallocate(other_procedure)
    end function checkFromProcedure

    function checkThroughModule() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Message_t, Info
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(Module_t), pointer :: another_module
        type(Procedure_t), pointer :: another_procedure
        class(Message_t), pointer :: message
        type(Module_t), pointer :: other_module
        type(Module_t), pointer :: the_module
        type(Procedure_t), pointer :: the_procedure

        the_module => Module_("Some_m")
        the_procedure => Procedure_("some")
        another_module => Module_("Another_m")
        another_procedure => Procedure_("another")
        other_module => Module_("Other_m")
        message => Info( &
                the_module, the_procedure, "Test Message")
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
        deallocate(message)
        deallocate(other_module)
    end function checkThroughModule

    function checkThroughProcedure() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Message_t, Info
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(Module_t), pointer :: another_module
        type(Procedure_t), pointer :: another_procedure
        class(Message_t), pointer :: message
        type(Procedure_t), pointer :: other_procedure
        type(Module_t), pointer :: the_module
        type(Procedure_t), pointer :: the_procedure

        the_module => Module_("Some_m")
        the_procedure => Procedure_("some")
        another_module => Module_("Another_m")
        another_procedure => Procedure_("another")
        other_procedure => Procedure_("other")
        message => Info( &
                the_module, the_procedure, "Test Message")
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
        deallocate(message)
        deallocate(other_procedure)
    end function checkThroughProcedure

    function checkContents() result(result_)
        use iso_varying_string, only: operator(//), var_str
        use Message_m, only: Message_t, Info
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        class(Message_t), pointer :: message

        message => Info( &
                Module_("Some_m"), Procedure_("some"), "Test Message Content")

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
        deallocate(message)
    end function checkContents
end module message_test

module message_test
    use iso_varying_string, only: VARYING_STRING, operator(//), var_str
    use Message_m, only: &
            Fatal, &
            Internal, &
            ERROR_TYPE, &
            FATAL_TYPE, &
            INPUTS_TYPE, &
            INTERNAL_TYPE, &
            OUTSIDE_NORMAL_RANGE_TYPE, &
            UNEQUAL_ARRAY_SIZES_TYPE, &
            UNKNOWN_TYPE_TYPE
    use erloff_debug_m, only: debug_t, DEBUG
    use erloff_debug_level_m, only: GENERAL
    use erloff_info_m, only: info_t, INFO
    use erloff_message_m, only: Message_t
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: procedure_t
    use erloff_warning_m, only: warning_t, WARNING
    use vegetables, only: &
            Result_t, &
            Test_Item_t, &
            Describe, &
            assert_Not, &
            assert_That, &
            It

    implicit none
    private

    public :: test_message
contains
    function test_message() result(tests)
        type(Test_Item_t) :: tests

        type(Test_Item_t) :: individual_tests(8)

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

        allocate(debug_message, source = debug_t( &
                INPUTS_TYPE, &
                module_t("Some_m"), &
                procedure_t("some"), &
                GENERAL, &
                "Test Message"))

        allocate(info_message, source = info_t( &
                UNEQUAL_ARRAY_SIZES_TYPE, &
                module_t("Some_m"), &
                procedure_t("some"), &
                "Test Message"))

        allocate(warning_message, source = warning_t( &
                OUTSIDE_NORMAL_RANGE_TYPE, &
                module_t("Some_m"), &
                procedure_t("some"), &
                "Test Message"))

        allocate(fatal_message, source = Fatal( &
                UNEQUAL_ARRAY_SIZES_TYPE, &
                module_t("Some_m"), &
                procedure_t("some"), &
                "Test Message"))

        allocate(internal_message, source = Internal( &
                UNKNOWN_TYPE_TYPE, &
                module_t("Some_m"), &
                procedure_t("some"), &
                "Test Message"))

        result_ = &
                assert_That( &
                        debug_message.isType.DEBUG, &
                        debug_message%repr() // ".isType." // DEBUG%repr()) &
                .and.assert_That( &
                        debug_message.isType.INPUTS_TYPE, &
                        debug_message%repr() // ".isType." // INPUTS_TYPE%repr()) &
                .and.assert_Not( &
                        debug_message.isType.INFO, &
                        debug_message%repr() // ".isType." // INFO%repr()) &
                .and.assert_Not( &
                        debug_message.isType.ERROR_TYPE, &
                        debug_message%repr() // ".isType." // ERROR_TYPE%repr()) &
                .and.assert_That( &
                        warning_message.isType.WARNING, &
                        warning_message%repr() // ".isType." // WARNING%repr()) &
                .and.assert_That( &
                        warning_message.isType.OUTSIDE_NORMAL_RANGE_TYPE, &
                        warning_message%repr() // ".isType." // OUTSIDE_NORMAL_RANGE_TYPE%repr()) &
                .and.assert_Not( &
                        warning_message.isType.INFO, &
                        warning_message%repr() // ".isType." // INFO%repr()) &
                .and.assert_Not( &
                        warning_message.isType.ERROR_TYPE, &
                        warning_message%repr() // ".isType." // ERROR_TYPE%repr()) &
                .and.assert_That( &
                        fatal_message.isType.ERROR_TYPE, &
                        fatal_message%repr() // ".isType." // ERROR_TYPE%repr()) &
                .and.assert_That( &
                        fatal_message.isType.FATAL_TYPE, &
                        fatal_message%repr() // ".isType." // FATAL_TYPE%repr()) &
                .and.assert_That( &
                        fatal_message.isType.UNEQUAL_ARRAY_SIZES_TYPE, &
                        fatal_message%repr() // ".isType." // UNEQUAL_ARRAY_SIZES_TYPE%repr()) &
                .and.assert_Not( &
                        fatal_message.isType.INTERNAL_TYPE, &
                        fatal_message%repr() // ".isType." // INTERNAL_TYPE%repr()) &
                .and.assert_That( &
                        internal_message.isType.ERROR_TYPE, &
                        internal_message%repr() // ".isType." // ERROR_TYPE%repr()) &
                .and.assert_That( &
                        internal_message.isType.INTERNAL_TYPE, &
                        internal_message%repr() // ".isType." // INTERNAL_TYPE%repr()) &
                .and.assert_That( &
                        internal_message.isType.UNKNOWN_TYPE_TYPE, &
                        internal_message%repr() // ".isType." // UNKNOWN_TYPE_TYPE%repr())
    end function checkType

    pure function checkOriginatingModule() result(result_)
        type(Result_t) :: result_

        type(Module_t) :: another_module
        type(procedure_t) :: another_procedure
        class(Message_t), allocatable :: message
        type(Module_t) :: other_module
        type(Module_t) :: the_module
        type(procedure_t) :: the_procedure

        the_module = module_t("Some_m")
        the_procedure = procedure_t("some")
        another_module = module_t("Another_m")
        another_procedure = procedure_t("another")
        other_module = module_t("Other_m")
        allocate(message, source = info_t( &
                the_module, the_procedure, "Test Message"))
        call message%prependNames(another_module, another_procedure)

        result_ = &
                assert_That( &
                        message.originatedFrom.the_module, &
                        message%repr() // '.originatedFrom.' // the_module%repr()) &
                .and.assert_Not( &
                        message.originatedFrom.another_module, &
                        message%repr() // '.originatedFrom.' // another_module%repr()) &
                .and.assert_Not( &
                        message.originatedFrom.other_module, &
                        message%repr() // '.originatedFrom.' // other_module%repr())
    end function checkOriginatingModule

    pure function checkOriginatingProcedure() result(result_)
        type(Result_t) :: result_

        type(module_t) :: another_module
        type(procedure_t) :: another_procedure
        class(Message_t), allocatable :: message
        type(procedure_t) :: other_procedure
        type(module_t) :: the_module
        type(procedure_t) :: the_procedure

        the_module = module_t("Some_m")
        the_procedure = procedure_t("some")
        another_module = module_t("Another_m")
        another_procedure = procedure_t("another")
        other_procedure = procedure_t("other")
        allocate(message, source = info_t( &
                the_module, the_procedure, "Test Message"))
        call message%prependNames(another_module, another_procedure)

        result_ = &
                assert_That( &
                        message.originatedFrom.the_procedure, &
                        message%repr() // '.originatedFrom.' // the_procedure%repr()) &
                .and.assert_Not( &
                        message.originatedFrom.another_procedure, &
                        message%repr() // '.originatedFrom.' // another_procedure%repr()) &
                .and.assert_Not( &
                        message.originatedFrom.other_procedure, &
                        message%repr() // '.originatedFrom.' // other_procedure%repr())
    end function checkOriginatingProcedure

    pure function checkFromModule() result(result_)
        type(Result_t) :: result_

        type(Module_t) :: another_module
        type(procedure_t) :: another_procedure
        class(Message_t), allocatable :: message
        type(Module_t) :: other_module
        type(Module_t) :: the_module
        type(procedure_t) :: the_procedure

        the_module = module_t("Some_m")
        the_procedure = procedure_t("some")
        another_module = module_t("Another_m")
        another_procedure = procedure_t("another")
        other_module = module_t("Other_m")
        allocate(message, source = info_t( &
                the_module, the_procedure, "Test Message"))
        call message%prependNames(another_module, another_procedure)

        result_ = &
                assert_That( &
                        message.isFrom.the_module, &
                        message%repr() // '.isFrom.' // the_module%repr()) &
                .and.assert_That( &
                        message.isFrom.another_module, &
                        message%repr() // '.isFrom.' // another_module%repr()) &
                .and.assert_Not( &
                        message.isFrom.other_module, &
                        message%repr() // '.isFrom.' // other_module%repr())
    end function checkFromModule

    pure function checkFromProcedure() result(result_)
        type(Result_t) :: result_

        type(Module_t) :: another_module
        type(procedure_t) :: another_procedure
        class(Message_t), allocatable :: message
        type(procedure_t) :: other_procedure
        type(Module_t) :: the_module
        type(procedure_t) :: the_procedure

        the_module = module_t("Some_m")
        the_procedure = procedure_t("some")
        another_module = module_t("Another_m")
        another_procedure = procedure_t("another")
        other_procedure = procedure_t("other")
        allocate(message, source = info_t( &
                the_module, the_procedure, "Test Message"))
        call message%prependNames(another_module, another_procedure)

        result_ = &
                assert_That( &
                        message.isFrom.the_procedure, &
                        message%repr() // '.isFrom.' // the_procedure%repr()) &
                .and.assert_That( &
                        message.isFrom.another_procedure, &
                        message%repr() // '.isFrom.' // another_procedure%repr()) &
                .and.assert_Not( &
                        message.isFrom.other_procedure, &
                        message%repr() // '.isFrom.' // other_procedure%repr())
    end function checkFromProcedure

    pure function checkThroughModule() result(result_)
        type(Result_t) :: result_

        type(Module_t) :: another_module
        type(procedure_t) :: another_procedure
        class(Message_t), allocatable :: message
        type(Module_t) :: other_module
        type(Module_t) :: the_module
        type(procedure_t) :: the_procedure

        the_module = module_t("Some_m")
        the_procedure = procedure_t("some")
        another_module = module_t("Another_m")
        another_procedure = procedure_t("another")
        other_module = module_t("Other_m")
        allocate(message, source = info_t( &
                the_module, the_procedure, "Test Message"))
        call message%prependNames(another_module, another_procedure)

        result_ = &
                assert_Not( &
                        message.cameThrough.the_module, &
                        message%repr() // '.cameThrough.' // the_module%repr()) &
                .and.assert_That( &
                        message.cameThrough.another_module, &
                        message%repr() // '.cameThrough.' // another_module%repr()) &
                .and.assert_Not( &
                        message.cameThrough.other_module, &
                        message%repr() // '.cameThrough.' // other_module%repr())
    end function checkThroughModule

    pure function checkThroughProcedure() result(result_)
        type(Result_t) :: result_

        type(Module_t) :: another_module
        type(procedure_t) :: another_procedure
        class(Message_t), allocatable :: message
        type(procedure_t) :: other_procedure
        type(Module_t) :: the_module
        type(procedure_t) :: the_procedure

        the_module = module_t("Some_m")
        the_procedure = procedure_t("some")
        another_module = module_t("Another_m")
        another_procedure = procedure_t("another")
        other_procedure = procedure_t("other")
        allocate(message, source = info_t( &
                the_module, the_procedure, "Test Message"))
        call message%prependNames(another_module, another_procedure)

        result_ = &
                assert_Not( &
                        message.cameThrough.the_procedure, &
                        message%repr() // '.cameThrough.' // the_procedure%repr()) &
                .and.assert_That( &
                        message.cameThrough.another_procedure, &
                        message%repr() // '.cameThrough.' // another_procedure%repr()) &
                .and.assert_Not( &
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

        allocate(message, source = info_t( &
                module_t("Some_m"), procedure_t("some"), "Test Message Content"))
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
                assert_That( &
                        message.includes."Test", &
                        message%repr() // '.includes."Test"') &
                .and.assert_Not( &
                        message.includes."test", &
                        message%repr() // '.includes."test"') &
                .and.assert_That( &
                        message.includesAnyOf.includesAnyStrings1, &
                        message%repr() // '.includesAnyOf.[var_str("Test"), var_str("else")]') &
                .and.assert_That( &
                        message.includesAnyOf.includesAnyStrings2, &
                        message%repr() // '.includesAnyOf.[var_str("Test"), var_str("Content")]') &
                .and.assert_Not( &
                        message.includesAnyOf.includesAnyStrings3, &
                        message%repr() // '.includesAnyOf.[var_str("test"), var_str("else")]') &
                .and.assert_That( &
                        message.includesAllOf.includesAllStrings1, &
                        message%repr() // '.includesAllOf.[var_str("Test"), var_str("Message")]') &
                .and.assert_Not( &
                        message.includesAllOf.includesAllStrings2, &
                        message%repr() // '.includesAllOf.[var_str("test"), var_str("Message")]') &
                .and.assert_Not( &
                        message.includesAllOf.includesAllStrings3, &
                        message%repr() // '.includesAllOf.[var_str("Test"), var_str("message")]')
    end function checkContents
end module message_test

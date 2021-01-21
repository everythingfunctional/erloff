module error_list_test
    use Error_list_m, only: ErrorList_t, size
    use iso_varying_string, only: VARYING_STRING, assignment(=), operator(//)
    use erloff_error_m, only: error_t
    use erloff_fatal_m, only: fatal_t, FATAL
    use erloff_internal_m, only: internal_t, INTERNAL
    use erloff_message_type_m, only: UNKNOWN_TYPE
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: procedure_t
    use vegetables, only: &
            Result_t, &
            Test_Item_t, &
            assert_Empty, &
            assert_Equals, &
            assert_Includes, &
            assert_Not, &
            assert_That, &
            Describe, &
            It

    implicit none
    private

    public :: test_error_list
contains
    function test_error_list() result(tests)
        type(Test_Item_t) :: tests

        type(Test_Item_t) :: individual_tests(22)

        individual_tests(1) = It( &
                "Converts to an empty string when it is empty", &
                checkEmptyToString)
        individual_tests(2) = It( &
                "Can append an error to an empty list", &
                checkAppendToEmpty)
        individual_tests(3) = It( &
                "Can append multiple to an empty list", &
                checkAppendMultipleToEmpty)
        individual_tests(4) = It( &
                "Can append an empty list", checkAppendEmpty)
        individual_tests(5) = It( &
                "Can combine two empty lists", checkCombineEmpty)
        individual_tests(6) = It( &
                "Can combine two lists", checkCombine)
        individual_tests(7) = It( &
                "Can filter errors by type", checkFilterByType)
        individual_tests(8) = It( &
                "Can filter errors by the originating module", &
                checkFilterByOriginatingModule)
        individual_tests(9) = It( &
                "Can filter errors by the originating procedure", &
                checkFilterByOriginatingProcedure)
        individual_tests(10) = It( &
                "Can filter errors by modules passed through", &
                checkFilterByModulesThrough)
        individual_tests(11) = It( &
                "Can filter errors by procedures passed through", &
                checkFilterByProceduresThrough)
        individual_tests(12) = It( &
                "Can filter errors by the modules they are from", &
                checkFilterByModulesFrom)
        individual_tests(13) = It( &
                "Can filter errors by the procedures they are from", &
                checkFilterByProceduresFrom)
        individual_tests(14) = It( &
                "Can filter errors based on their contents", &
                checkFilterByContents)
        individual_tests(15) = It( &
                "Can tell if it has an error of a given type", &
                checkForType)
        individual_tests(16) = It( &
                "Can tell if it has an error originating from a module", &
                checkForOriginatingModule)
        individual_tests(17) = It( &
                "Can tell if it has an error originating from a procedure", &
                checkForOriginatingProcedure)
        individual_tests(18) = It( &
                "Can tell if it has an error coming through a module", &
                checkForThroughModule)
        individual_tests(19) = It( &
                "Can tell if it has an error coming through a procedure", &
                checkForThroughProcedure)
        individual_tests(20) = It( &
                "Can tell if it has an error coming from a module", &
                checkForFromModule)
        individual_tests(21) = It( &
                "Can tell if it has an error comming from a procedure", &
                checkForFromProcedure)
        individual_tests(22) = It( &
                "Can tell if it has an error with some contents", &
                checkForContents)
        tests = Describe("ErrorList_t", individual_tests)
    end function test_error_list

    pure function checkEmptyToString() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: error_list

        result_ = assert_Empty(error_list%toString())
    end function checkEmptyToString

    pure function checkAppendToEmpty() result(result_)
        type(Result_t) :: result_

        class(Error_t), allocatable :: error
        type(ErrorList_t) :: error_list

        allocate(error, source = fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Test Error"))
        call error_list%appendError(error)

        result_ = assert_Includes(error%to_string(), error_list%toString())
    end function checkAppendToEmpty

    function checkAppendMultipleToEmpty() result(result_)
        type(Result_t) :: result_

        class(Error_t), allocatable :: error1
        class(Error_t), allocatable :: error2
        type(ErrorList_t) :: error_list1
        type(ErrorList_t) :: error_list2

        allocate(error1, source = fatal_t( &
                module_t("Some_m"), procedure_t("some"), "First Error"))
        allocate(error2, source = internal_t( &
                module_t("Some_m"), procedure_t("some"), "Second Error"))
        call error_list1%appendError(error1)
        call error_list1%appendError(error2)

        call error_list2%appendErrors( &
                error_list1, module_t("Another_m"), procedure_t("another"))

        result_ = &
                assert_Includes(error1%to_string(), error_list2%toString()) &
                .and.assert_Includes(error2%to_string(), error_list2%toString())
    end function checkAppendMultipleToEmpty

    function checkAppendEmpty() result(result_)
        type(Result_t) :: result_

        class(Error_t), allocatable :: error1
        class(Error_t), allocatable :: error2
        type(ErrorList_t) :: error_list1
        type(ErrorList_t) :: error_list2

        allocate(error1, source = fatal_t( &
                module_t("Some_m"), procedure_t("some"), "First Error"))
        allocate(error2, source = internal_t( &
                module_t("Some_m"), procedure_t("some"), "Second Error"))
        call error_list1%appendError(error1)
        call error_list1%appendError(error2)

        call error_list1%appendErrors( &
                error_list2, module_t("Another_m"), procedure_t("another"))

        result_ = &
                assert_Includes(error1%to_string(), error_list1%toString()) &
                .and.assert_Includes(error2%to_string(), error_list1%toString())
    end function checkAppendEmpty

    function checkCombineEmpty() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: error_list1
        type(ErrorList_t) :: error_list2

        call error_list1%appendErrors( &
                error_list2, module_t("Another_m"), procedure_t("another"))

        result_ = assert_Empty(error_list1%toString())
    end function checkCombineEmpty

    function checkCombine() result(result_)
        type(Result_t) :: result_

        class(Error_t), allocatable :: error1
        class(Error_t), allocatable :: error2
        class(Error_t), allocatable :: error3
        class(Error_t), allocatable :: error4
        type(ErrorList_t) :: error_list1
        type(ErrorList_t) :: error_list2

        allocate(error1, source = fatal_t( &
                module_t("Some_m"), procedure_t("some"), "First Error"))
        allocate(error2, source = internal_t( &
                module_t("Some_m"), procedure_t("some"), "Second Error"))
        call error_list1%appendError(error1)
        call error_list1%appendError(error2)

        allocate(error3, source = fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Third Error"))
        allocate(error4, source = internal_t( &
                module_t("Some_m"), procedure_t("some"), "Fourth Error"))
        call error_list2%appendError(error3)
        call error_list2%appendError(error4)

        call error_list1%appendErrors( &
                error_list2, module_t("Another_m"), procedure_t("another"))

        result_ = &
                assert_Includes(error1%to_string(), error_list1%toString()) &
                .and.assert_Includes(error2%to_string(), error_list1%toString()) &
                .and.assert_Includes(error3%to_string(), error_list1%toString()) &
                .and.assert_Includes(error4%to_string(), error_list1%toString())
    end function checkCombine

    pure function checkFilterByType() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: errors

        call errors%appendError(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Test error"))
        call errors%appendError(internal_t( &
                module_t("Some_m"), procedure_t("some"), "Test warning"))
        call errors%appendError(fatal_t( &
                UNKNOWN_TYPE, module_t("Some_m"), procedure_t("some"), "Test error"))

        result_ = &
                assert_Equals(1, size(errors.ofType.INTERNAL), "INTERNAL") &
                .and.assert_Equals( &
                        2, &
                        size(errors.ofTypes.[INTERNAL, UNKNOWN_TYPE]), &
                        "INTERNAL or UNKNOWN_TYPE")
    end function checkFilterByType

    pure function checkFilterByOriginatingModule() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Module_t) :: module1
        type(Module_t) :: module2
        type(Module_t) :: module3
        type(procedure_t) :: procedure1
        type(procedure_t) :: procedure2
        type(procedure_t) :: procedure3

        module1 = module_t("Some_m")
        module2 = module_t("Another_m")
        module3 = module_t("Yet_another_m")
        procedure1 = procedure_t("some")
        procedure2 = procedure_t("another")
        procedure3 = procedure_t("yetAnother")

        call errors%appendError(fatal_t( &
                module1, procedure1, "Test error"))
        call errors%appendError(fatal_t( &
                module2, procedure2, "Another error"))
        call errors%appendError(fatal_t( &
                module3, procedure3, "Yet another error"))

        result_ = &
                assert_Equals( &
                        1, &
                        size(errors.originatingFrom.module1), &
                        module1%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(errors.originatingFrom.module2), &
                        module2%repr()) &
                .and.assert_Equals( &
                        2, &
                        size(errors.originatingFrom.[module1, module2]), &
                        module1%repr() // " or " // module2%repr())
    end function checkFilterByOriginatingModule

    pure function checkFilterByOriginatingProcedure() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Module_t) :: module1
        type(Module_t) :: module2
        type(Module_t) :: module3
        type(procedure_t) :: procedure1
        type(procedure_t) :: procedure2
        type(procedure_t) :: procedure3

        module1 = module_t("Some_m")
        module2 = module_t("Another_m")
        module3 = module_t("Yet_another_m")
        procedure1 = procedure_t("some")
        procedure2 = procedure_t("another")
        procedure3 = procedure_t("yetAnother")

        call errors%appendError(fatal_t( &
                module1, procedure1, "Test error"))
        call errors%appendError(fatal_t( &
                module2, procedure2, "Another error"))
        call errors%appendError(fatal_t( &
                module3, procedure3, "Yet another error"))

        result_ = &
                assert_Equals( &
                        1, &
                        size(errors.originatingFrom.procedure1), &
                        procedure1%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(errors.originatingFrom.procedure2), &
                        procedure2%repr()) &
                .and.assert_Equals( &
                        2, &
                        size(errors.originatingFrom.[procedure1, procedure2]), &
                        procedure1%repr() // " or " // procedure2%repr())
    end function checkFilterByOriginatingProcedure

    function checkFilterByModulesThrough() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: branch1_bottom_errors
        type(ErrorList_t) :: branch1_middle_errors
        type(ErrorList_t) :: branch2_bottom_errors
        type(ErrorList_t) :: branch2_middle_errors
        type(ErrorList_t) :: branch3_bottom_errors
        type(ErrorList_t) :: branch3_middle_errors
        type(ErrorList_t) :: top_level_errors
        type(Module_t) :: branch1_bottom_module
        type(Module_t) :: branch1_middle_module
        type(Module_t) :: branch2_bottom_module
        type(Module_t) :: branch2_middle_module
        type(Module_t) :: branch3_bottom_module
        type(Module_t) :: branch3_middle_module
        type(Module_t) :: top_level_module
        type(procedure_t) :: branch1_bottom_procedure
        type(procedure_t) :: branch1_middle_procedure
        type(procedure_t) :: branch2_bottom_procedure
        type(procedure_t) :: branch2_middle_procedure
        type(procedure_t) :: branch3_bottom_procedure
        type(procedure_t) :: branch3_middle_procedure
        type(procedure_t) :: top_level_procedure

        branch1_bottom_module = module_t("Branch1_originating_m")
        branch1_middle_module = module_t("Branch1_middle_m")
        branch2_bottom_module = module_t("Branch2_originating_m")
        branch2_middle_module = module_t("Branch2_middle_m")
        branch3_bottom_module = module_t("Branch3_originating_m")
        branch3_middle_module = module_t("Branch3_middle_m")
        top_level_module = module_t("Top_level_m")
        branch1_bottom_procedure = procedure_t("branch1Originating")
        branch1_middle_procedure = procedure_t("branch1Middle")
        branch2_bottom_procedure = procedure_t("branch2Originating")
        branch2_middle_procedure = procedure_t("branch2Middle")
        branch3_bottom_procedure = procedure_t("branch3Originating")
        branch3_middle_procedure = procedure_t("branch3Middle")
        top_level_procedure = procedure_t("topLevel")

        call branch1_bottom_errors%appendError(fatal_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        call branch1_middle_errors%appendErrors( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_errors%appendError(fatal_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        call branch2_middle_errors%appendErrors( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_errors%appendError(fatal_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "error"))
        call branch3_middle_errors%appendErrors( &
                branch3_bottom_errors, &
                branch3_middle_module, &
                branch3_middle_procedure)
        call top_level_errors%appendErrors( &
                branch1_middle_errors, &
                top_level_module, &
                top_level_procedure)
        call top_level_errors%appendErrors( &
                branch2_middle_errors, &
                top_level_module, &
                top_level_procedure)
        call top_level_errors%appendErrors( &
                branch3_middle_errors, &
                top_level_module, &
                top_level_procedure)

        result_ = &
                assert_Equals( &
                        0, &
                        size(top_level_errors.comingThrough.branch1_bottom_module), &
                        branch1_bottom_module%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(top_level_errors.comingThrough.branch1_middle_module), &
                        branch1_middle_module%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(top_level_errors.comingThrough.branch2_middle_module), &
                        branch2_middle_module%repr()) &
                .and.assert_Equals( &
                        2, &
                        size(top_level_errors.comingThrough.[branch1_middle_module, branch2_middle_module]), &
                        branch1_middle_module%repr() // " or " // branch2_middle_module%repr())
    end function checkFilterByModulesThrough

    function checkFilterByProceduresThrough() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: branch1_bottom_errors
        type(ErrorList_t) :: branch1_middle_errors
        type(ErrorList_t) :: branch2_bottom_errors
        type(ErrorList_t) :: branch2_middle_errors
        type(ErrorList_t) :: branch3_bottom_errors
        type(ErrorList_t) :: branch3_middle_errors
        type(ErrorList_t) :: top_level_errors
        type(Module_t) :: branch1_bottom_module
        type(Module_t) :: branch1_middle_module
        type(Module_t) :: branch2_bottom_module
        type(Module_t) :: branch2_middle_module
        type(Module_t) :: branch3_bottom_module
        type(Module_t) :: branch3_middle_module
        type(Module_t) :: top_level_module
        type(procedure_t) :: branch1_bottom_procedure
        type(procedure_t) :: branch1_middle_procedure
        type(procedure_t) :: branch2_bottom_procedure
        type(procedure_t) :: branch2_middle_procedure
        type(procedure_t) :: branch3_bottom_procedure
        type(procedure_t) :: branch3_middle_procedure
        type(procedure_t) :: top_level_procedure

        branch1_bottom_module = module_t("Branch1_originating_m")
        branch1_middle_module = module_t("Branch1_middle_m")
        branch2_bottom_module = module_t("Branch2_originating_m")
        branch2_middle_module = module_t("Branch2_middle_m")
        branch3_bottom_module = module_t("Branch3_originating_m")
        branch3_middle_module = module_t("Branch3_middle_m")
        top_level_module = module_t("Top_level_m")
        branch1_bottom_procedure = procedure_t("branch1Originating")
        branch1_middle_procedure = procedure_t("branch1Middle")
        branch2_bottom_procedure = procedure_t("branch2Originating")
        branch2_middle_procedure = procedure_t("branch2Middle")
        branch3_bottom_procedure = procedure_t("branch3Originating")
        branch3_middle_procedure = procedure_t("branch3Middle")
        top_level_procedure = procedure_t("topLevel")

        call branch1_bottom_errors%appendError(fatal_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        call branch1_middle_errors%appendErrors( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_errors%appendError(fatal_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        call branch2_middle_errors%appendErrors( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_errors%appendError(fatal_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "error"))
        call branch3_middle_errors%appendErrors( &
                branch3_bottom_errors, &
                branch3_middle_module, &
                branch3_middle_procedure)
        call top_level_errors%appendErrors( &
                branch1_middle_errors, &
                top_level_module, &
                top_level_procedure)
        call top_level_errors%appendErrors( &
                branch2_middle_errors, &
                top_level_module, &
                top_level_procedure)
        call top_level_errors%appendErrors( &
                branch3_middle_errors, &
                top_level_module, &
                top_level_procedure)

        result_ = &
                assert_Equals( &
                        0, &
                        size(top_level_errors.comingThrough.branch1_bottom_procedure), &
                        branch1_bottom_procedure%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(top_level_errors.comingThrough.branch1_middle_procedure), &
                        branch1_middle_procedure%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(top_level_errors.comingThrough.branch2_middle_procedure), &
                        branch2_middle_procedure%repr()) &
                .and.assert_Equals( &
                        2, &
                        size(top_level_errors.comingThrough.[branch1_middle_procedure, branch2_middle_procedure]), &
                        branch1_middle_procedure%repr() // " or " // branch2_middle_procedure%repr())
    end function checkFilterByProceduresThrough

    function checkFilterByModulesFrom() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: branch1_bottom_errors
        type(ErrorList_t) :: branch1_middle_errors
        type(ErrorList_t) :: branch2_bottom_errors
        type(ErrorList_t) :: branch2_middle_errors
        type(ErrorList_t) :: branch3_bottom_errors
        type(ErrorList_t) :: branch3_middle_errors
        type(ErrorList_t) :: top_level_errors
        type(Module_t) :: branch1_bottom_module
        type(Module_t) :: branch1_middle_module
        type(Module_t) :: branch2_bottom_module
        type(Module_t) :: branch2_middle_module
        type(Module_t) :: branch3_bottom_module
        type(Module_t) :: branch3_middle_module
        type(Module_t) :: top_level_module
        type(procedure_t) :: branch1_bottom_procedure
        type(procedure_t) :: branch1_middle_procedure
        type(procedure_t) :: branch2_bottom_procedure
        type(procedure_t) :: branch2_middle_procedure
        type(procedure_t) :: branch3_bottom_procedure
        type(procedure_t) :: branch3_middle_procedure
        type(procedure_t) :: top_level_procedure

        branch1_bottom_module = module_t("Branch1_originating_m")
        branch1_middle_module = module_t("Branch1_middle_m")
        branch2_bottom_module = module_t("Branch2_originating_m")
        branch2_middle_module = module_t("Branch2_middle_m")
        branch3_bottom_module = module_t("Branch3_originating_m")
        branch3_middle_module = module_t("Branch3_middle_m")
        top_level_module = module_t("Top_level_m")
        branch1_bottom_procedure = procedure_t("branch1Originating")
        branch1_middle_procedure = procedure_t("branch1Middle")
        branch2_bottom_procedure = procedure_t("branch2Originating")
        branch2_middle_procedure = procedure_t("branch2Middle")
        branch3_bottom_procedure = procedure_t("branch3Originating")
        branch3_middle_procedure = procedure_t("branch3Middle")
        top_level_procedure = procedure_t("topLevel")

        call branch1_bottom_errors%appendError(fatal_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        call branch1_middle_errors%appendErrors( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_errors%appendError(fatal_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        call branch2_middle_errors%appendErrors( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_errors%appendError(fatal_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "error"))
        call branch3_middle_errors%appendErrors( &
                branch3_bottom_errors, &
                branch3_middle_module, &
                branch3_middle_procedure)
        call top_level_errors%appendErrors( &
                branch1_middle_errors, &
                top_level_module, &
                top_level_procedure)
        call top_level_errors%appendErrors( &
                branch2_middle_errors, &
                top_level_module, &
                top_level_procedure)
        call top_level_errors%appendErrors( &
                branch3_middle_errors, &
                top_level_module, &
                top_level_procedure)

        result_ = &
                assert_Equals( &
                        1, &
                        size(top_level_errors.from.branch1_bottom_module), &
                        branch1_bottom_module%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(top_level_errors.from.branch1_middle_module), &
                        branch1_middle_module%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(top_level_errors.from.branch2_middle_module), &
                        branch2_middle_module%repr()) &
                .and.assert_Equals( &
                        2, &
                        size(top_level_errors.from.[branch1_middle_module, branch2_middle_module]), &
                        branch1_middle_module%repr() // " or " // branch2_middle_module%repr())
    end function checkFilterByModulesFrom

    function checkFilterByProceduresFrom() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: branch1_bottom_errors
        type(ErrorList_t) :: branch1_middle_errors
        type(ErrorList_t) :: branch2_bottom_errors
        type(ErrorList_t) :: branch2_middle_errors
        type(ErrorList_t) :: branch3_bottom_errors
        type(ErrorList_t) :: branch3_middle_errors
        type(ErrorList_t) :: top_level_errors
        type(Module_t) :: branch1_bottom_module
        type(Module_t) :: branch1_middle_module
        type(Module_t) :: branch2_bottom_module
        type(Module_t) :: branch2_middle_module
        type(Module_t) :: branch3_bottom_module
        type(Module_t) :: branch3_middle_module
        type(Module_t) :: top_level_module
        type(procedure_t) :: branch1_bottom_procedure
        type(procedure_t) :: branch1_middle_procedure
        type(procedure_t) :: branch2_bottom_procedure
        type(procedure_t) :: branch2_middle_procedure
        type(procedure_t) :: branch3_bottom_procedure
        type(procedure_t) :: branch3_middle_procedure
        type(procedure_t) :: top_level_procedure

        branch1_bottom_module = module_t("Branch1_originating_m")
        branch1_middle_module = module_t("Branch1_middle_m")
        branch2_bottom_module = module_t("Branch2_originating_m")
        branch2_middle_module = module_t("Branch2_middle_m")
        branch3_bottom_module = module_t("Branch3_originating_m")
        branch3_middle_module = module_t("Branch3_middle_m")
        top_level_module = module_t("Top_level_m")
        branch1_bottom_procedure = procedure_t("branch1Originating")
        branch1_middle_procedure = procedure_t("branch1Middle")
        branch2_bottom_procedure = procedure_t("branch2Originating")
        branch2_middle_procedure = procedure_t("branch2Middle")
        branch3_bottom_procedure = procedure_t("branch3Originating")
        branch3_middle_procedure = procedure_t("branch3Middle")
        top_level_procedure = procedure_t("topLevel")

        call branch1_bottom_errors%appendError(fatal_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        call branch1_middle_errors%appendErrors( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_errors%appendError(fatal_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        call branch2_middle_errors%appendErrors( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_errors%appendError(fatal_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "error"))
        call branch3_middle_errors%appendErrors( &
                branch3_bottom_errors, &
                branch3_middle_module, &
                branch3_middle_procedure)
        call top_level_errors%appendErrors( &
                branch1_middle_errors, &
                top_level_module, &
                top_level_procedure)
        call top_level_errors%appendErrors( &
                branch2_middle_errors, &
                top_level_module, &
                top_level_procedure)
        call top_level_errors%appendErrors( &
                branch3_middle_errors, &
                top_level_module, &
                top_level_procedure)

        result_ = &
                assert_Equals( &
                        1, &
                        size(top_level_errors.from.branch1_bottom_procedure), &
                        branch1_bottom_procedure%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(top_level_errors.from.branch1_middle_procedure), &
                        branch1_middle_procedure%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(top_level_errors.from.branch2_middle_procedure), &
                        branch2_middle_procedure%repr()) &
                .and.assert_Equals( &
                        2, &
                        size(top_level_errors.from.[branch1_middle_procedure, branch2_middle_procedure]), &
                        branch1_middle_procedure%repr() // " or " // branch2_middle_procedure%repr())
    end function checkFilterByProceduresFrom

    pure function checkFilterByContents() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(VARYING_STRING) :: test_string1
        type(VARYING_STRING) :: test_string2

        test_string1 = "Hello"
        test_string2 = "Test"

        call errors%appendError(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Hello Test"))
        call errors%appendError(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Goodbye Test"))
        call errors%appendError(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Example Error"))
        call errors%appendError(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Simple Error"))
        call errors%appendError(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Test Error"))
        call errors%appendError(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Hello Error"))

        result_ = &
                assert_Equals( &
                        2, &
                        size(errors.including."Hello"), &
                        'including "Hello"') &
                .and.assert_Equals( &
                        3, &
                        size(errors.including.test_string2), &
                        'including "' // test_string2 // '"') &
                .and.assert_Equals( &
                        4, &
                        size(errors.includingAnyOf.[test_string1, test_string2]), &
                        'includingAnyOf "' // test_string1 // '" or "' // test_string2 // '"') &
                .and.assert_Equals( &
                        1, &
                        size(errors.includingAllOf.[test_string1, test_string2]), &
                        'includingAllOf "' // test_string1 // '" or "' // test_string2 // '"')
    end function checkFilterByContents

    pure function checkForType() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: empty_list
        type(ErrorList_t) :: errors

        call errors%appendError(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Test Error"))

        result_ = &
                assert_Not( &
                        empty_list.hasType.FATAL, &
                        empty_list%repr() // ".hasType." // FATAL%repr()) &
                .and.assert_That( &
                        errors.hasType.FATAL, &
                        errors%repr() // ".hasType." // FATAL%repr())
    end function checkForType

    pure function checkForOriginatingModule() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: empty_list
        type(ErrorList_t) :: errors

        type(Module_t) :: module1
        type(Module_t) :: module2
        type(procedure_t) :: procedure1
        type(procedure_t) :: procedure2

        module1 = module_t("Some_m")
        module2 = module_t("Another_m")
        procedure1 = procedure_t("some")
        procedure2 = procedure_t("another")

        call errors%appendError(fatal_t( &
                module1, procedure1, "Test error"))
        call errors%appendError(fatal_t( &
                module2, procedure2, "Another error"))

        result_ = &
                assert_Not( &
                        empty_list.hasAnyOriginatingFrom.module1, &
                        empty_list%repr() // ".hasAnyOriginatingFrom." // module1%repr()) &
                .and.assert_That( &
                        errors.hasAnyOriginatingFrom.module1, &
                        errors%repr() // ".hasAnyOriginatingFrom." // module1%repr())
    end function checkForOriginatingModule

    pure function checkForOriginatingProcedure() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: empty_list
        type(ErrorList_t) :: errors

        type(Module_t) :: module1
        type(Module_t) :: module2
        type(procedure_t) :: procedure1
        type(procedure_t) :: procedure2

        module1 = module_t("Some_m")
        module2 = module_t("Another_m")
        procedure1 = procedure_t("some")
        procedure2 = procedure_t("another")

        call errors%appendError(fatal_t( &
                module1, procedure1, "Test error"))
        call errors%appendError(fatal_t( &
                module2, procedure2, "Another error"))

        result_ = &
                assert_Not( &
                        empty_list.hasAnyOriginatingFrom.procedure1, &
                        empty_list%repr() // ".hasAnyOriginatingFrom." // procedure1%repr()) &
                .and.assert_That( &
                        errors.hasAnyOriginatingFrom.procedure1, &
                        errors%repr() // ".hasAnyOriginatingFrom." // procedure1%repr())
    end function checkForOriginatingProcedure

    function checkForThroughModule() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: branch1_bottom_errors
        type(ErrorList_t) :: branch1_middle_errors
        type(ErrorList_t) :: branch2_bottom_errors
        type(ErrorList_t) :: branch2_middle_errors
        type(ErrorList_t) :: branch3_bottom_errors
        type(ErrorList_t) :: branch3_middle_errors
        type(ErrorList_t) :: top_level_errors
        type(Module_t) :: branch1_bottom_module
        type(Module_t) :: branch1_middle_module
        type(Module_t) :: branch2_bottom_module
        type(Module_t) :: branch2_middle_module
        type(Module_t) :: branch3_bottom_module
        type(Module_t) :: branch3_middle_module
        type(Module_t) :: top_level_module
        type(procedure_t) :: branch1_bottom_procedure
        type(procedure_t) :: branch1_middle_procedure
        type(procedure_t) :: branch2_bottom_procedure
        type(procedure_t) :: branch2_middle_procedure
        type(procedure_t) :: branch3_bottom_procedure
        type(procedure_t) :: branch3_middle_procedure
        type(procedure_t) :: top_level_procedure

        branch1_bottom_module = module_t("Branch1_originating_m")
        branch1_middle_module = module_t("Branch1_middle_m")
        branch2_bottom_module = module_t("Branch2_originating_m")
        branch2_middle_module = module_t("Branch2_middle_m")
        branch3_bottom_module = module_t("Branch3_originating_m")
        branch3_middle_module = module_t("Branch3_middle_m")
        top_level_module = module_t("Top_level_m")
        branch1_bottom_procedure = procedure_t("branch1Originating")
        branch1_middle_procedure = procedure_t("branch1Middle")
        branch2_bottom_procedure = procedure_t("branch2Originating")
        branch2_middle_procedure = procedure_t("branch2Middle")
        branch3_bottom_procedure = procedure_t("branch3Originating")
        branch3_middle_procedure = procedure_t("branch3Middle")
        top_level_procedure = procedure_t("topLevel")

        call branch1_bottom_errors%appendError(fatal_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        call branch1_middle_errors%appendErrors( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_errors%appendError(fatal_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        call branch2_middle_errors%appendErrors( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_errors%appendError(fatal_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "error"))
        call branch3_middle_errors%appendErrors( &
                branch3_bottom_errors, &
                branch3_middle_module, &
                branch3_middle_procedure)
        call top_level_errors%appendErrors( &
                branch1_middle_errors, &
                top_level_module, &
                top_level_procedure)
        call top_level_errors%appendErrors( &
                branch2_middle_errors, &
                top_level_module, &
                top_level_procedure)
        call top_level_errors%appendErrors( &
                branch3_middle_errors, &
                top_level_module, &
                top_level_procedure)

        result_ = &
                assert_That( &
                        top_level_errors.hasAnyComingThrough.branch1_middle_module, &
                        top_level_errors%repr() // ".hasAnyCominghThrough." // branch1_middle_module%repr()) &
                .and.assert_Not( &
                        top_level_errors.hasAnyComingThrough.branch1_bottom_module, &
                        top_level_errors%repr() // ".hasAnyCominghThrough." // branch1_bottom_module%repr())
    end function checkForThroughModule

    function checkForThroughProcedure() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: branch1_bottom_errors
        type(ErrorList_t) :: branch1_middle_errors
        type(ErrorList_t) :: branch2_bottom_errors
        type(ErrorList_t) :: branch2_middle_errors
        type(ErrorList_t) :: branch3_bottom_errors
        type(ErrorList_t) :: branch3_middle_errors
        type(ErrorList_t) :: top_level_errors
        type(Module_t) :: branch1_bottom_module
        type(Module_t) :: branch1_middle_module
        type(Module_t) :: branch2_bottom_module
        type(Module_t) :: branch2_middle_module
        type(Module_t) :: branch3_bottom_module
        type(Module_t) :: branch3_middle_module
        type(Module_t) :: top_level_module
        type(procedure_t) :: branch1_bottom_procedure
        type(procedure_t) :: branch1_middle_procedure
        type(procedure_t) :: branch2_bottom_procedure
        type(procedure_t) :: branch2_middle_procedure
        type(procedure_t) :: branch3_bottom_procedure
        type(procedure_t) :: branch3_middle_procedure
        type(procedure_t) :: top_level_procedure

        branch1_bottom_module = module_t("Branch1_originating_m")
        branch1_middle_module = module_t("Branch1_middle_m")
        branch2_bottom_module = module_t("Branch2_originating_m")
        branch2_middle_module = module_t("Branch2_middle_m")
        branch3_bottom_module = module_t("Branch3_originating_m")
        branch3_middle_module = module_t("Branch3_middle_m")
        top_level_module = module_t("Top_level_m")
        branch1_bottom_procedure = procedure_t("branch1Originating")
        branch1_middle_procedure = procedure_t("branch1Middle")
        branch2_bottom_procedure = procedure_t("branch2Originating")
        branch2_middle_procedure = procedure_t("branch2Middle")
        branch3_bottom_procedure = procedure_t("branch3Originating")
        branch3_middle_procedure = procedure_t("branch3Middle")
        top_level_procedure = procedure_t("topLevel")

        call branch1_bottom_errors%appendError(fatal_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        call branch1_middle_errors%appendErrors( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_errors%appendError(fatal_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        call branch2_middle_errors%appendErrors( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_errors%appendError(fatal_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "error"))
        call branch3_middle_errors%appendErrors( &
                branch3_bottom_errors, &
                branch3_middle_module, &
                branch3_middle_procedure)
        call top_level_errors%appendErrors( &
                branch1_middle_errors, &
                top_level_module, &
                top_level_procedure)
        call top_level_errors%appendErrors( &
                branch2_middle_errors, &
                top_level_module, &
                top_level_procedure)
        call top_level_errors%appendErrors( &
                branch3_middle_errors, &
                top_level_module, &
                top_level_procedure)

        result_ = &
                assert_That( &
                        top_level_errors.hasAnyComingThrough.branch1_middle_procedure, &
                        top_level_errors%repr() // ".hasAnyCominghThrough." // branch1_middle_procedure%repr()) &
                .and.assert_Not( &
                        top_level_errors.hasAnyComingThrough.branch1_bottom_procedure, &
                        top_level_errors%repr() // ".hasAnyCominghThrough." // branch1_bottom_procedure%repr())
    end function checkForThroughProcedure

    function checkForFromModule() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: branch1_bottom_errors
        type(ErrorList_t) :: branch1_middle_errors
        type(ErrorList_t) :: branch2_bottom_errors
        type(ErrorList_t) :: branch2_middle_errors
        type(ErrorList_t) :: branch3_bottom_errors
        type(ErrorList_t) :: branch3_middle_errors
        type(ErrorList_t) :: top_level_errors
        type(Module_t) :: branch1_bottom_module
        type(Module_t) :: branch1_middle_module
        type(Module_t) :: branch2_bottom_module
        type(Module_t) :: branch2_middle_module
        type(Module_t) :: branch3_bottom_module
        type(Module_t) :: branch3_middle_module
        type(Module_t) :: top_level_module
        type(procedure_t) :: branch1_bottom_procedure
        type(procedure_t) :: branch1_middle_procedure
        type(procedure_t) :: branch2_bottom_procedure
        type(procedure_t) :: branch2_middle_procedure
        type(procedure_t) :: branch3_bottom_procedure
        type(procedure_t) :: branch3_middle_procedure
        type(procedure_t) :: top_level_procedure

        branch1_bottom_module = module_t("Branch1_originating_m")
        branch1_middle_module = module_t("Branch1_middle_m")
        branch2_bottom_module = module_t("Branch2_originating_m")
        branch2_middle_module = module_t("Branch2_middle_m")
        branch3_bottom_module = module_t("Branch3_originating_m")
        branch3_middle_module = module_t("Branch3_middle_m")
        top_level_module = module_t("Top_level_m")
        branch1_bottom_procedure = procedure_t("branch1Originating")
        branch1_middle_procedure = procedure_t("branch1Middle")
        branch2_bottom_procedure = procedure_t("branch2Originating")
        branch2_middle_procedure = procedure_t("branch2Middle")
        branch3_bottom_procedure = procedure_t("branch3Originating")
        branch3_middle_procedure = procedure_t("branch3Middle")
        top_level_procedure = procedure_t("topLevel")

        call branch1_bottom_errors%appendError(fatal_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        call branch1_middle_errors%appendErrors( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_errors%appendError(fatal_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        call branch2_middle_errors%appendErrors( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_errors%appendError(fatal_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "error"))
        call branch3_middle_errors%appendErrors( &
                branch3_bottom_errors, &
                branch3_middle_module, &
                branch3_middle_procedure)
        call top_level_errors%appendErrors( &
                branch1_middle_errors, &
                top_level_module, &
                top_level_procedure)
        call top_level_errors%appendErrors( &
                branch2_middle_errors, &
                top_level_module, &
                top_level_procedure)
        call top_level_errors%appendErrors( &
                branch3_middle_errors, &
                top_level_module, &
                top_level_procedure)

        result_ = &
                assert_That( &
                        top_level_errors.hasAnyFrom.branch1_middle_module, &
                        top_level_errors%repr() // ".hasAnyFrom." // branch1_middle_module%repr()) &
                .and.assert_That( &
                        top_level_errors.hasAnyFrom.branch1_bottom_module, &
                        top_level_errors%repr() // ".hasAnyFrom." // branch1_bottom_module%repr())
    end function checkForFromModule

    function checkForFromProcedure() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: branch1_bottom_errors
        type(ErrorList_t) :: branch1_middle_errors
        type(ErrorList_t) :: branch2_bottom_errors
        type(ErrorList_t) :: branch2_middle_errors
        type(ErrorList_t) :: branch3_bottom_errors
        type(ErrorList_t) :: branch3_middle_errors
        type(ErrorList_t) :: top_level_errors
        type(Module_t) :: branch1_bottom_module
        type(Module_t) :: branch1_middle_module
        type(Module_t) :: branch2_bottom_module
        type(Module_t) :: branch2_middle_module
        type(Module_t) :: branch3_bottom_module
        type(Module_t) :: branch3_middle_module
        type(Module_t) :: top_level_module
        type(procedure_t) :: branch1_bottom_procedure
        type(procedure_t) :: branch1_middle_procedure
        type(procedure_t) :: branch2_bottom_procedure
        type(procedure_t) :: branch2_middle_procedure
        type(procedure_t) :: branch3_bottom_procedure
        type(procedure_t) :: branch3_middle_procedure
        type(procedure_t) :: top_level_procedure

        branch1_bottom_module = module_t("Branch1_originating_m")
        branch1_middle_module = module_t("Branch1_middle_m")
        branch2_bottom_module = module_t("Branch2_originating_m")
        branch2_middle_module = module_t("Branch2_middle_m")
        branch3_bottom_module = module_t("Branch3_originating_m")
        branch3_middle_module = module_t("Branch3_middle_m")
        top_level_module = module_t("Top_level_m")
        branch1_bottom_procedure = procedure_t("branch1Originating")
        branch1_middle_procedure = procedure_t("branch1Middle")
        branch2_bottom_procedure = procedure_t("branch2Originating")
        branch2_middle_procedure = procedure_t("branch2Middle")
        branch3_bottom_procedure = procedure_t("branch3Originating")
        branch3_middle_procedure = procedure_t("branch3Middle")
        top_level_procedure = procedure_t("topLevel")

        call branch1_bottom_errors%appendError(fatal_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        call branch1_middle_errors%appendErrors( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_errors%appendError(fatal_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        call branch2_middle_errors%appendErrors( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_errors%appendError(fatal_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "error"))
        call branch3_middle_errors%appendErrors( &
                branch3_bottom_errors, &
                branch3_middle_module, &
                branch3_middle_procedure)
        call top_level_errors%appendErrors( &
                branch1_middle_errors, &
                top_level_module, &
                top_level_procedure)
        call top_level_errors%appendErrors( &
                branch2_middle_errors, &
                top_level_module, &
                top_level_procedure)
        call top_level_errors%appendErrors( &
                branch3_middle_errors, &
                top_level_module, &
                top_level_procedure)

        result_ = &
                assert_That( &
                        top_level_errors.hasAnyFrom.branch1_middle_procedure, &
                        top_level_errors%repr() // ".hasAnyFrom." // branch1_middle_procedure%repr()) &
                .and.assert_That( &
                        top_level_errors.hasAnyFrom.branch1_bottom_procedure, &
                        top_level_errors%repr() // ".hasAnyFrom." // branch1_bottom_procedure%repr())
    end function checkForFromProcedure

    pure function checkForContents() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(VARYING_STRING) :: test_string1
        type(VARYING_STRING) :: test_string2
        type(VARYING_STRING) :: test_string3

        test_string1 = "Hello"
        test_string2 = "Test"
        test_string3 = "Other"

        call errors%appendError(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Hello Test"))
        call errors%appendError(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Goodbye Test"))
        call errors%appendError(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Example Error"))
        call errors%appendError(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Simple Error"))
        call errors%appendError(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Test Error"))
        call errors%appendError(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Hello Error"))

        result_ = &
                assert_That( &
                        errors.hasAnyIncluding."Hello", &
                        errors%repr() // '.hasAnyIncluding."Hello"') &
                .and.assert_Not( &
                        errors.hasAnyIncluding.test_string3, &
                        errors%repr() // '.hasAnyIncluding."' // test_string3 // '"') &
                .and.assert_That( &
                        errors.hasAnyIncludingAnyOf.[test_string1, test_string2], &
                        errors%repr() // '.hasAnyIncludingAnyOf."' // test_string1 // '" or "' // test_string2 // '"') &
                .and.assert_That( &
                        errors.hasAnyIncludingAllOf.[test_string1, test_string2], &
                        errors%repr() // '.hasAnyIncludingAllOf."' // test_string1 // '" or "' // test_string2 // '"')
    end function checkForContents
end module error_list_test

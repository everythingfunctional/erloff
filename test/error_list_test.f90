module error_list_test
    use iso_varying_string, only: VARYING_STRING, assignment(=), operator(//)
    use erloff_error_list_m, only: error_list_t, size
    use erloff_fatal_m, only: fatal_t, FATAL
    use erloff_internal_m, only: internal_t, INTERNAL
    use erloff_message_type_m, only: UNKNOWN_TYPE
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: procedure_t
    use veggies, only: &
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

        tests = describe( &
                "error_list_t", &
                [ it( &
                        "Converts to an empty string when it is empty", &
                        checkEmptyToString) &
                , it( &
                        "Can append an error to an empty list", &
                        checkAppendToEmpty) &
                , it( &
                        "Can append multiple to an empty list", &
                        checkAppendMultipleToEmpty) &
                , it("Can append an empty list", checkAppendEmpty) &
                , it("Can combine two empty lists", checkCombineEmpty) &
                , it("Can combine two lists", checkCombine) &
                , it("can create a new list from multiple", checkNewFromMultiple) &
                , it("can append content to messages in a list", check_append_content) &
                , it("can prepend content to messages in a list", check_prepend_content) &
                , it("Can filter errors by type", checkFilterByType) &
                , it( &
                        "Can filter errors by the originating module", &
                        checkFilterByOriginatingModule) &
                , it( &
                        "Can filter errors by the originating procedure", &
                        checkFilterByOriginatingProcedure) &
                , it( &
                        "Can filter errors by modules passed through", &
                        checkFilterByModulesThrough) &
                , it( &
                        "Can filter errors by procedures passed through", &
                        checkFilterByProceduresThrough) &
                , it( &
                        "Can filter errors by the modules they are from", &
                        checkFilterByModulesFrom) &
                , it( &
                        "Can filter errors by the procedures they are from", &
                        checkFilterByProceduresFrom) &
                , it( &
                        "Can filter errors based on their contents", &
                        checkFilterByContents) &
                , it( &
                        "Can tell if it has an error of a given type", &
                        checkForType) &
                , it( &
                        "Can tell if it has an error originating from a module", &
                        checkForOriginatingModule) &
                , it( &
                        "Can tell if it has an error originating from a procedure", &
                        checkForOriginatingProcedure) &
                , it( &
                        "Can tell if it has an error coming through a module", &
                        checkForThroughModule) &
                , it( &
                        "Can tell if it has an error coming through a procedure", &
                        checkForThroughProcedure) &
                , it( &
                        "Can tell if it has an error coming from a module", &
                        checkForFromModule) &
                , it( &
                        "Can tell if it has an error comming from a procedure", &
                        checkForFromProcedure) &
                , it( &
                        "Can tell if it has an error with some contents", &
                        checkForContents) &
                ])
    end function test_error_list

    function checkEmptyToString() result(result_)
        type(Result_t) :: result_

        type(error_list_t) :: error_list

        result_ = assert_Empty(error_list%to_string())
    end function checkEmptyToString

    function checkAppendToEmpty() result(result_)
        type(Result_t) :: result_

        type(fatal_t) :: error
        type(error_list_t) :: error_list

        error = fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Test Error")
        error_list = error_list%with_error_appended(error)

        result_ = assert_Includes(error%to_string(), error_list%to_string())
    end function checkAppendToEmpty

    function checkAppendMultipleToEmpty() result(result_)
        type(Result_t) :: result_

        type(fatal_t) :: error1
        type(internal_t) :: error2
        type(error_list_t) :: error_list1
        type(error_list_t) :: error_list2

        error1 = fatal_t( &
                module_t("Some_m"), procedure_t("some"), "First Error")
        error2 = internal_t( &
                module_t("Some_m"), procedure_t("some"), "Second Error")
        error_list1 = error_list1%with_error_appended(error1)
        error_list1 = error_list1%with_error_appended(error2)

        error_list2 = error_list2%with_errors_appended( &
                error_list1, module_t("Another_m"), procedure_t("another"))

        result_ = &
                assert_Includes(error1%to_string(), error_list2%to_string()) &
                .and.assert_Includes(error2%to_string(), error_list2%to_string())
    end function checkAppendMultipleToEmpty

    function checkAppendEmpty() result(result_)
        type(Result_t) :: result_

        type(fatal_t) :: error1
        type(internal_t) :: error2
        type(error_list_t) :: error_list1
        type(error_list_t) :: error_list2

        error1 = fatal_t( &
                module_t("Some_m"), procedure_t("some"), "First Error")
        error2 = internal_t( &
                module_t("Some_m"), procedure_t("some"), "Second Error")
                error_list1 = error_list1%with_error_appended(error1)
                error_list1 = error_list1%with_error_appended(error2)

        error_list1 = error_list1%with_errors_appended( &
                error_list2, module_t("Another_m"), procedure_t("another"))

        result_ = &
                assert_Includes(error1%to_string(), error_list1%to_string()) &
                .and.assert_Includes(error2%to_string(), error_list1%to_string())
    end function checkAppendEmpty

    function checkCombineEmpty() result(result_)
        type(Result_t) :: result_

        type(error_list_t) :: error_list1
        type(error_list_t) :: error_list2

        error_list1 = error_list1%with_errors_appended( &
                error_list2, module_t("Another_m"), procedure_t("another"))

        result_ = assert_Empty(error_list1%to_string())
    end function checkCombineEmpty

    function checkCombine() result(result_)
        type(Result_t) :: result_

        type(fatal_t) :: error1
        type(internal_t) :: error2
        type(fatal_t) :: error3
        type(internal_t) :: error4
        type(error_list_t) :: error_list1
        type(error_list_t) :: error_list2

        error1 = fatal_t( &
                module_t("Some_m"), procedure_t("some"), "First Error")
        error2 = internal_t( &
                module_t("Some_m"), procedure_t("some"), "Second Error")
        error_list1 = error_list1%with_error_appended(error1)
        error_list1 = error_list1%with_error_appended(error2)

        error3 = fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Third Error")
        error4 = internal_t( &
                module_t("Some_m"), procedure_t("some"), "Fourth Error")
        error_list2 = error_list2%with_error_appended(error3)
        error_list2 = error_list2%with_error_appended(error4)

        error_list1 = error_list1%with_errors_appended( &
                error_list2, module_t("Another_m"), procedure_t("another"))

        result_ = &
                assert_Includes(error1%to_string(), error_list1%to_string()) &
                .and.assert_Includes(error2%to_string(), error_list1%to_string()) &
                .and.assert_Includes(error3%to_string(), error_list1%to_string()) &
                .and.assert_Includes(error4%to_string(), error_list1%to_string())
    end function checkCombine

    function checkNewFromMultiple() result(result_)
        type(result_t) :: result_

        type(fatal_t) :: error1
        type(internal_t) :: error2
        type(fatal_t) :: error3
        type(internal_t) :: error4
        type(error_list_t) :: error_list1
        type(error_list_t) :: error_list2
        type(error_list_t) :: combined

        error1 = fatal_t( &
                module_t("Some_m"), procedure_t("some"), "First Error")
        error2 = internal_t( &
                module_t("Some_m"), procedure_t("some"), "Second Error")
        error_list1 = error_list1%with_error_appended(error1)
        error_list1 = error_list1%with_error_appended(error2)

        error3 = fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Third Error")
        error4 = internal_t( &
                module_t("Some_m"), procedure_t("some"), "Fourth Error")
        error_list2 = error_list2%with_error_appended(error3)
        error_list2 = error_list2%with_error_appended(error4)

        combined = error_list_t( &
                [error_list1, error_list2], &
                module_t("Another_m"), &
                procedure_t("another"))

        result_ = &
                assert_Includes(error1%to_string(), combined%to_string()) &
                .and.assert_Includes(error2%to_string(), combined%to_string()) &
                .and.assert_Includes(error3%to_string(), combined%to_string()) &
                .and.assert_Includes(error4%to_string(), combined%to_string())
    end function

    function check_append_content() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: extra_stuff = " extra stuff"
        type(error_list_t) :: errors
        type(error_list_t) :: with_content

        errors = errors%with_error_appended(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Test error"))
        errors = errors%with_error_appended(internal_t( &
                module_t("Some_m"), procedure_t("some"), "Test warning"))
        errors = errors%with_error_appended(fatal_t( &
                UNKNOWN_TYPE, module_t("Some_m"), procedure_t("some"), "Test error"))

        with_content = errors%with_content_appended(extra_stuff)

        result_ = assert_includes(extra_stuff, with_content%to_string())
    end function

    function check_prepend_content() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: extra_stuff = "Extra stuff "
        type(error_list_t) :: errors
        type(error_list_t) :: with_content

        errors = errors%with_error_appended(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Test error"))
        errors = errors%with_error_appended(internal_t( &
                module_t("Some_m"), procedure_t("some"), "Test warning"))
        errors = errors%with_error_appended(fatal_t( &
                UNKNOWN_TYPE, module_t("Some_m"), procedure_t("some"), "Test error"))

        with_content = errors%with_content_prepended(extra_stuff)

        result_ = assert_includes(extra_stuff, with_content%to_string())
    end function

    function checkFilterByType() result(result_)
        type(Result_t) :: result_

        type(error_list_t) :: errors

        errors = errors%with_error_appended(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Test error"))
        errors = errors%with_error_appended(internal_t( &
                module_t("Some_m"), procedure_t("some"), "Test warning"))
        errors = errors%with_error_appended(fatal_t( &
                UNKNOWN_TYPE, module_t("Some_m"), procedure_t("some"), "Test error"))

        result_ = &
                assert_Equals(1, size(errors.ofType.INTERNAL), "INTERNAL") &
                .and.assert_Equals( &
                        2, &
                        size(errors.ofTypes.[INTERNAL, UNKNOWN_TYPE]), &
                        "INTERNAL or UNKNOWN_TYPE")
    end function checkFilterByType

    function checkFilterByOriginatingModule() result(result_)
        type(Result_t) :: result_

        type(error_list_t) :: errors
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

        errors = errors%with_error_appended(fatal_t( &
                module1, procedure1, "Test error"))
        errors = errors%with_error_appended(fatal_t( &
                module2, procedure2, "Another error"))
        errors = errors%with_error_appended(fatal_t( &
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

    function checkFilterByOriginatingProcedure() result(result_)
        type(Result_t) :: result_

        type(error_list_t) :: errors
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

        errors = errors%with_error_appended(fatal_t( &
                module1, procedure1, "Test error"))
        errors = errors%with_error_appended(fatal_t( &
                module2, procedure2, "Another error"))
        errors = errors%with_error_appended(fatal_t( &
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

        type(error_list_t) :: branch1_bottom_errors
        type(error_list_t) :: branch1_middle_errors
        type(error_list_t) :: branch2_bottom_errors
        type(error_list_t) :: branch2_middle_errors
        type(error_list_t) :: branch3_bottom_errors
        type(error_list_t) :: branch3_middle_errors
        type(error_list_t) :: top_level_errors
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

        branch1_bottom_errors = error_list_t(fatal_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        branch1_middle_errors = error_list_t( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        branch2_bottom_errors = error_list_t(fatal_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        branch2_middle_errors = error_list_t( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        branch3_bottom_errors = error_list_t(fatal_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "error"))
        branch3_middle_errors = error_list_t( &
                branch3_bottom_errors, &
                branch3_middle_module, &
                branch3_middle_procedure)
        top_level_errors = error_list_t( &
                branch1_middle_errors, &
                top_level_module, &
                top_level_procedure)
        top_level_errors = top_level_errors%with_errors_appended( &
                branch2_middle_errors, &
                top_level_module, &
                top_level_procedure)
        top_level_errors = top_level_errors%with_errors_appended( &
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

        type(error_list_t) :: branch1_bottom_errors
        type(error_list_t) :: branch1_middle_errors
        type(error_list_t) :: branch2_bottom_errors
        type(error_list_t) :: branch2_middle_errors
        type(error_list_t) :: branch3_bottom_errors
        type(error_list_t) :: branch3_middle_errors
        type(error_list_t) :: top_level_errors
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

        branch1_bottom_errors = error_list_t(fatal_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        branch1_middle_errors = error_list_t( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        branch2_bottom_errors = error_list_t(fatal_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        branch2_middle_errors = error_list_t( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        branch3_bottom_errors = error_list_t(fatal_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "error"))
        branch3_middle_errors = error_list_t( &
                branch3_bottom_errors, &
                branch3_middle_module, &
                branch3_middle_procedure)
        top_level_errors = error_list_t( &
                branch1_middle_errors, &
                top_level_module, &
                top_level_procedure)
        top_level_errors = top_level_errors%with_errors_appended( &
                branch2_middle_errors, &
                top_level_module, &
                top_level_procedure)
        top_level_errors = top_level_errors%with_errors_appended( &
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

        type(error_list_t) :: branch1_bottom_errors
        type(error_list_t) :: branch1_middle_errors
        type(error_list_t) :: branch2_bottom_errors
        type(error_list_t) :: branch2_middle_errors
        type(error_list_t) :: branch3_bottom_errors
        type(error_list_t) :: branch3_middle_errors
        type(error_list_t) :: top_level_errors
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

        branch1_bottom_errors = error_list_t(fatal_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        branch1_middle_errors = error_list_t( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        branch2_bottom_errors = error_list_t(fatal_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        branch2_middle_errors = error_list_t( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        branch3_bottom_errors = error_list_t(fatal_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "error"))
        branch3_middle_errors = error_list_t( &
                branch3_bottom_errors, &
                branch3_middle_module, &
                branch3_middle_procedure)
        top_level_errors = error_list_t( &
                branch1_middle_errors, &
                top_level_module, &
                top_level_procedure)
        top_level_errors = top_level_errors%with_errors_appended( &
                branch2_middle_errors, &
                top_level_module, &
                top_level_procedure)
        top_level_errors = top_level_errors%with_errors_appended( &
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

        type(error_list_t) :: branch1_bottom_errors
        type(error_list_t) :: branch1_middle_errors
        type(error_list_t) :: branch2_bottom_errors
        type(error_list_t) :: branch2_middle_errors
        type(error_list_t) :: branch3_bottom_errors
        type(error_list_t) :: branch3_middle_errors
        type(error_list_t) :: top_level_errors
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

        branch1_bottom_errors = error_list_t(fatal_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        branch1_middle_errors = error_list_t( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        branch2_bottom_errors = error_list_t(fatal_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        branch2_middle_errors = error_list_t( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        branch3_bottom_errors = error_list_t(fatal_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "error"))
        branch3_middle_errors = error_list_t( &
                branch3_bottom_errors, &
                branch3_middle_module, &
                branch3_middle_procedure)
        top_level_errors = error_list_t( &
                branch1_middle_errors, &
                top_level_module, &
                top_level_procedure)
        top_level_errors = top_level_errors%with_errors_appended( &
                branch2_middle_errors, &
                top_level_module, &
                top_level_procedure)
        top_level_errors = top_level_errors%with_errors_appended( &
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

    function checkFilterByContents() result(result_)
        type(Result_t) :: result_

        type(error_list_t) :: errors
        type(VARYING_STRING) :: test_string1
        type(VARYING_STRING) :: test_string2

        test_string1 = "Hello"
        test_string2 = "Test"

        errors = errors%with_error_appended(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Hello Test"))
        errors = errors%with_error_appended(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Goodbye Test"))
        errors = errors%with_error_appended(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Example Error"))
        errors = errors%with_error_appended(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Simple Error"))
        errors = errors%with_error_appended(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Test Error"))
        errors = errors%with_error_appended(fatal_t( &
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

    function checkForType() result(result_)
        type(Result_t) :: result_

        type(error_list_t) :: empty_list
        type(error_list_t) :: errors

        errors = error_list_t(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Test Error"))

        result_ = &
                assert_Not( &
                        empty_list.hasType.FATAL, &
                        empty_list%repr() // ".hasType." // FATAL%repr()) &
                .and.assert_That( &
                        errors.hasType.FATAL, &
                        errors%repr() // ".hasType." // FATAL%repr())
    end function checkForType

    function checkForOriginatingModule() result(result_)
        type(Result_t) :: result_

        type(error_list_t) :: empty_list
        type(error_list_t) :: errors

        type(Module_t) :: module1
        type(Module_t) :: module2
        type(procedure_t) :: procedure1
        type(procedure_t) :: procedure2

        module1 = module_t("Some_m")
        module2 = module_t("Another_m")
        procedure1 = procedure_t("some")
        procedure2 = procedure_t("another")

        errors = errors%with_error_appended(fatal_t( &
                module1, procedure1, "Test error"))
        errors = errors%with_error_appended(fatal_t( &
                module2, procedure2, "Another error"))

        result_ = &
                assert_Not( &
                        empty_list.hasAnyOriginatingFrom.module1, &
                        empty_list%repr() // ".hasAnyOriginatingFrom." // module1%repr()) &
                .and.assert_That( &
                        errors.hasAnyOriginatingFrom.module1, &
                        errors%repr() // ".hasAnyOriginatingFrom." // module1%repr())
    end function checkForOriginatingModule

    function checkForOriginatingProcedure() result(result_)
        type(Result_t) :: result_

        type(error_list_t) :: empty_list
        type(error_list_t) :: errors

        type(Module_t) :: module1
        type(Module_t) :: module2
        type(procedure_t) :: procedure1
        type(procedure_t) :: procedure2

        module1 = module_t("Some_m")
        module2 = module_t("Another_m")
        procedure1 = procedure_t("some")
        procedure2 = procedure_t("another")

        errors = errors%with_error_appended(fatal_t( &
                module1, procedure1, "Test error"))
        errors = errors%with_error_appended(fatal_t( &
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

        type(error_list_t) :: branch1_bottom_errors
        type(error_list_t) :: branch1_middle_errors
        type(error_list_t) :: branch2_bottom_errors
        type(error_list_t) :: branch2_middle_errors
        type(error_list_t) :: branch3_bottom_errors
        type(error_list_t) :: branch3_middle_errors
        type(error_list_t) :: top_level_errors
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

        branch1_bottom_errors = error_list_t(fatal_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        branch1_middle_errors = error_list_t( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        branch2_bottom_errors = error_list_t(fatal_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        branch2_middle_errors = error_list_t( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        branch3_bottom_errors = error_list_t(fatal_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "error"))
        branch3_middle_errors = error_list_t( &
                branch3_bottom_errors, &
                branch3_middle_module, &
                branch3_middle_procedure)
        top_level_errors = error_list_t( &
                branch1_middle_errors, &
                top_level_module, &
                top_level_procedure)
        top_level_errors = top_level_errors%with_errors_appended( &
                branch2_middle_errors, &
                top_level_module, &
                top_level_procedure)
        top_level_errors = top_level_errors%with_errors_appended( &
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

        type(error_list_t) :: branch1_bottom_errors
        type(error_list_t) :: branch1_middle_errors
        type(error_list_t) :: branch2_bottom_errors
        type(error_list_t) :: branch2_middle_errors
        type(error_list_t) :: branch3_bottom_errors
        type(error_list_t) :: branch3_middle_errors
        type(error_list_t) :: top_level_errors
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

        branch1_bottom_errors = error_list_t(fatal_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        branch1_middle_errors = error_list_t( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        branch2_bottom_errors = error_list_t(fatal_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        branch2_middle_errors = error_list_t( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        branch3_bottom_errors = error_list_t(fatal_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "error"))
        branch3_middle_errors = error_list_t( &
                branch3_bottom_errors, &
                branch3_middle_module, &
                branch3_middle_procedure)
        top_level_errors = error_list_t( &
                branch1_middle_errors, &
                top_level_module, &
                top_level_procedure)
        top_level_errors = top_level_errors%with_errors_appended( &
                branch2_middle_errors, &
                top_level_module, &
                top_level_procedure)
        top_level_errors = top_level_errors%with_errors_appended( &
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

        type(error_list_t) :: branch1_bottom_errors
        type(error_list_t) :: branch1_middle_errors
        type(error_list_t) :: branch2_bottom_errors
        type(error_list_t) :: branch2_middle_errors
        type(error_list_t) :: branch3_bottom_errors
        type(error_list_t) :: branch3_middle_errors
        type(error_list_t) :: top_level_errors
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

        branch1_bottom_errors = error_list_t(fatal_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        branch1_middle_errors = error_list_t( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        branch2_bottom_errors = error_list_t(fatal_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        branch2_middle_errors = error_list_t( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        branch3_bottom_errors = error_list_t(fatal_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "error"))
        branch3_middle_errors = error_list_t( &
                branch3_bottom_errors, &
                branch3_middle_module, &
                branch3_middle_procedure)
        top_level_errors = error_list_t( &
                branch1_middle_errors, &
                top_level_module, &
                top_level_procedure)
        top_level_errors = top_level_errors%with_errors_appended( &
                branch2_middle_errors, &
                top_level_module, &
                top_level_procedure)
        top_level_errors = top_level_errors%with_errors_appended( &
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

        type(error_list_t) :: branch1_bottom_errors
        type(error_list_t) :: branch1_middle_errors
        type(error_list_t) :: branch2_bottom_errors
        type(error_list_t) :: branch2_middle_errors
        type(error_list_t) :: branch3_bottom_errors
        type(error_list_t) :: branch3_middle_errors
        type(error_list_t) :: top_level_errors
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

        branch1_bottom_errors = error_list_t(fatal_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        branch1_middle_errors = error_list_t( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        branch2_bottom_errors = error_list_t(fatal_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        branch2_middle_errors = error_list_t( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        branch3_bottom_errors = error_list_t(fatal_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "error"))
        branch3_middle_errors = error_list_t( &
                branch3_bottom_errors, &
                branch3_middle_module, &
                branch3_middle_procedure)
        top_level_errors = error_list_t( &
                branch1_middle_errors, &
                top_level_module, &
                top_level_procedure)
        top_level_errors = top_level_errors%with_errors_appended( &
                branch2_middle_errors, &
                top_level_module, &
                top_level_procedure)
        top_level_errors = top_level_errors%with_errors_appended( &
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

    function checkForContents() result(result_)
        type(Result_t) :: result_

        type(error_list_t) :: errors
        type(VARYING_STRING) :: test_string1
        type(VARYING_STRING) :: test_string2
        type(VARYING_STRING) :: test_string3

        test_string1 = "Hello"
        test_string2 = "Test"
        test_string3 = "Other"

        errors = errors%with_error_appended(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Hello Test"))
        errors = errors%with_error_appended(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Goodbye Test"))
        errors = errors%with_error_appended(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Example Error"))
        errors = errors%with_error_appended(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Simple Error"))
        errors = errors%with_error_appended(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Test Error"))
        errors = errors%with_error_appended(fatal_t( &
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

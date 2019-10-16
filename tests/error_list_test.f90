module error_list_test
    implicit none
    private

    public :: test_error_list
contains
    function test_error_list() result(tests)
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(22)

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

    function checkEmptyToString() result(result_)
        use Error_list_m, only: ErrorList_t
        use Vegetables_m, only: Result_t, assertEmpty

        type(Result_t) :: result_

        type(ErrorList_t) :: error_list

        result_ = assertEmpty(error_list%toString())
    end function checkEmptyToString

    function checkAppendToEmpty() result(result_)
        use Message_m, only: Error_t, Fatal
        use Error_list_m, only: ErrorList_t
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use Vegetables_m, only: Result_t, assertIncludes

        type(Result_t) :: result_

        class(Error_t), allocatable :: error
        type(ErrorList_t) :: error_list

        allocate(error, source = Fatal( &
                Module_("Some_m"), Procedure_("some"), "Test Error"))
        call error_list%appendError(error)

        result_ = assertIncludes(error%toString(), error_list%toString())
    end function checkAppendToEmpty

    function checkAppendMultipleToEmpty() result(result_)
        use Message_m, only: Error_t, Fatal, Internal
        use Error_list_m, only: ErrorList_t
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use Vegetables_m, only: Result_t, assertIncludes

        type(Result_t) :: result_

        class(Error_t), allocatable :: error1
        class(Error_t), allocatable :: error2
        type(ErrorList_t) :: error_list1
        type(ErrorList_t) :: error_list2

        allocate(error1, source = Fatal( &
                Module_("Some_m"), Procedure_("some"), "First Error"))
        allocate(error2, source = Internal( &
                Module_("Some_m"), Procedure_("some"), "Second Error"))
        call error_list1%appendError(error1)
        call error_list1%appendError(error2)

        call error_list2%appendErrors( &
                error_list1, Module_("Another_m"), Procedure_("another"))

        result_ = &
                assertIncludes(error1%toString(), error_list2%toString()) &
                .and.assertIncludes(error2%toString(), error_list2%toString())
    end function checkAppendMultipleToEmpty

    function checkAppendEmpty() result(result_)
        use Message_m, only: Error_t, Fatal, Internal
        use Error_list_m, only: ErrorList_t
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use Vegetables_m, only: Result_t, assertIncludes

        type(Result_t) :: result_

        class(Error_t), allocatable :: error1
        class(Error_t), allocatable :: error2
        type(ErrorList_t) :: error_list1
        type(ErrorList_t) :: error_list2

        allocate(error1, source = Fatal( &
                Module_("Some_m"), Procedure_("some"), "First Error"))
        allocate(error2, source = Internal( &
                Module_("Some_m"), Procedure_("some"), "Second Error"))
        call error_list1%appendError(error1)
        call error_list1%appendError(error2)

        call error_list1%appendErrors( &
                error_list2, Module_("Another_m"), Procedure_("another"))

        result_ = &
                assertIncludes(error1%toString(), error_list1%toString()) &
                .and.assertIncludes(error2%toString(), error_list1%toString())
    end function checkAppendEmpty

    function checkCombineEmpty() result(result_)
        use Error_list_m, only: ErrorList_t
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use Vegetables_m, only: Result_t, assertEmpty

        type(Result_t) :: result_

        type(ErrorList_t) :: error_list1
        type(ErrorList_t) :: error_list2

        call error_list1%appendErrors( &
                error_list2, Module_("Another_m"), Procedure_("another"))

        result_ = assertEmpty(error_list1%toString())
    end function checkCombineEmpty

    function checkCombine() result(result_)
        use Message_m, only: Error_t, Fatal, Internal
        use Error_list_m, only: ErrorList_t
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use Vegetables_m, only: Result_t, assertIncludes

        type(Result_t) :: result_

        class(Error_t), allocatable :: error1
        class(Error_t), allocatable :: error2
        class(Error_t), allocatable :: error3
        class(Error_t), allocatable :: error4
        type(ErrorList_t) :: error_list1
        type(ErrorList_t) :: error_list2

        allocate(error1, source = Fatal( &
                Module_("Some_m"), Procedure_("some"), "First Error"))
        allocate(error2, source = Internal( &
                Module_("Some_m"), Procedure_("some"), "Second Error"))
        call error_list1%appendError(error1)
        call error_list1%appendError(error2)

        allocate(error3, source = Fatal( &
                Module_("Some_m"), Procedure_("some"), "Third Error"))
        allocate(error4, source = Internal( &
                Module_("Some_m"), Procedure_("some"), "Fourth Error"))
        call error_list2%appendError(error3)
        call error_list2%appendError(error4)

        call error_list1%appendErrors( &
                error_list2, Module_("Another_m"), Procedure_("another"))

        result_ = &
                assertIncludes(error1%toString(), error_list1%toString()) &
                .and.assertIncludes(error2%toString(), error_list1%toString()) &
                .and.assertIncludes(error3%toString(), error_list1%toString()) &
                .and.assertIncludes(error4%toString(), error_list1%toString())
    end function checkCombine

    function checkFilterByType() result(result_)
        use Message_m, only: &
                Fatal, Internal, INTERNAL_TYPE, UNKNOWN_TYPE_TYPE
        use Error_list_m, only: ErrorList_t, size
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(ErrorList_t) :: errors

        call errors%appendError(Fatal( &
                Module_("Some_m"), Procedure_("some"), "Test error"))
        call errors%appendError(Internal( &
                Module_("Some_m"), Procedure_("some"), "Test warning"))
        call errors%appendError(Fatal( &
                UNKNOWN_TYPE_TYPE, Module_("Some_m"), Procedure_("some"), "Test error"))

        result_ = &
                assertEquals(1, size(errors.ofType.INTERNAL_TYPE), "INTERNAL") &
                .and.assertEquals( &
                        2, &
                        size(errors.ofTypes.[INTERNAL_TYPE, UNKNOWN_TYPE_TYPE]), &
                        "INTERNAL or UNKNOWN_TYPE")
    end function checkFilterByType

    function checkFilterByOriginatingModule() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Fatal
        use Error_list_m, only: ErrorList_t, size
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Module_t) :: module1
        type(Module_t) :: module2
        type(Module_t) :: module3
        type(Procedure_t) :: procedure1
        type(Procedure_t) :: procedure2
        type(Procedure_t) :: procedure3

        module1 = Module_("Some_m")
        module2 = Module_("Another_m")
        module3 = Module_("Yet_another_m")
        procedure1 = Procedure_("some")
        procedure2 = Procedure_("another")
        procedure3 = Procedure_("yetAnother")

        call errors%appendError(Fatal( &
                module1, procedure1, "Test error"))
        call errors%appendError(Fatal( &
                module2, procedure2, "Another error"))
        call errors%appendError(Fatal( &
                module3, procedure3, "Yet another error"))

        result_ = &
                assertEquals( &
                        1, &
                        size(errors.originatingFrom.module1), &
                        module1%repr()) &
                .and.assertEquals( &
                        1, &
                        size(errors.originatingFrom.module2), &
                        module2%repr()) &
                .and.assertEquals( &
                        2, &
                        size(errors.originatingFrom.[module1, module2]), &
                        module1%repr() // " or " // module2%repr())
    end function checkFilterByOriginatingModule

    function checkFilterByOriginatingProcedure() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Fatal
        use Error_list_m, only: ErrorList_t, size
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Module_t) :: module1
        type(Module_t) :: module2
        type(Module_t) :: module3
        type(Procedure_t) :: procedure1
        type(Procedure_t) :: procedure2
        type(Procedure_t) :: procedure3

        module1 = Module_("Some_m")
        module2 = Module_("Another_m")
        module3 = Module_("Yet_another_m")
        procedure1 = Procedure_("some")
        procedure2 = Procedure_("another")
        procedure3 = Procedure_("yetAnother")

        call errors%appendError(Fatal( &
                module1, procedure1, "Test error"))
        call errors%appendError(Fatal( &
                module2, procedure2, "Another error"))
        call errors%appendError(Fatal( &
                module3, procedure3, "Yet another error"))

        result_ = &
                assertEquals( &
                        1, &
                        size(errors.originatingFrom.procedure1), &
                        procedure1%repr()) &
                .and.assertEquals( &
                        1, &
                        size(errors.originatingFrom.procedure2), &
                        procedure2%repr()) &
                .and.assertEquals( &
                        2, &
                        size(errors.originatingFrom.[procedure1, procedure2]), &
                        procedure1%repr() // " or " // procedure2%repr())
    end function checkFilterByOriginatingProcedure

    function checkFilterByModulesThrough() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Fatal
        use Error_list_m, only: ErrorList_t, size
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertEquals

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
        type(Procedure_t) :: branch1_bottom_procedure
        type(Procedure_t) :: branch1_middle_procedure
        type(Procedure_t) :: branch2_bottom_procedure
        type(Procedure_t) :: branch2_middle_procedure
        type(Procedure_t) :: branch3_bottom_procedure
        type(Procedure_t) :: branch3_middle_procedure
        type(Procedure_t) :: top_level_procedure

        branch1_bottom_module = Module_("Branch1_originating_m")
        branch1_middle_module = Module_("Branch1_middle_m")
        branch2_bottom_module = Module_("Branch2_originating_m")
        branch2_middle_module = Module_("Branch2_middle_m")
        branch3_bottom_module = Module_("Branch3_originating_m")
        branch3_middle_module = Module_("Branch3_middle_m")
        top_level_module = Module_("Top_level_m")
        branch1_bottom_procedure = Procedure_("branch1Originating")
        branch1_middle_procedure = Procedure_("branch1Middle")
        branch2_bottom_procedure = Procedure_("branch2Originating")
        branch2_middle_procedure = Procedure_("branch2Middle")
        branch3_bottom_procedure = Procedure_("branch3Originating")
        branch3_middle_procedure = Procedure_("branch3Middle")
        top_level_procedure = Procedure_("topLevel")

        call branch1_bottom_errors%appendError(Fatal( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        call branch1_middle_errors%appendErrors( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_errors%appendError(Fatal( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        call branch2_middle_errors%appendErrors( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_errors%appendError(Fatal( &
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
                assertEquals( &
                        0, &
                        size(top_level_errors.comingThrough.branch1_bottom_module), &
                        branch1_bottom_module%repr()) &
                .and.assertEquals( &
                        1, &
                        size(top_level_errors.comingThrough.branch1_middle_module), &
                        branch1_middle_module%repr()) &
                .and.assertEquals( &
                        1, &
                        size(top_level_errors.comingThrough.branch2_middle_module), &
                        branch2_middle_module%repr()) &
                .and.assertEquals( &
                        2, &
                        size(top_level_errors.comingThrough.[branch1_middle_module, branch2_middle_module]), &
                        branch1_middle_module%repr() // " or " // branch2_middle_module%repr())
    end function checkFilterByModulesThrough

    function checkFilterByProceduresThrough() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Fatal
        use Error_list_m, only: ErrorList_t, size
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertEquals

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
        type(Procedure_t) :: branch1_bottom_procedure
        type(Procedure_t) :: branch1_middle_procedure
        type(Procedure_t) :: branch2_bottom_procedure
        type(Procedure_t) :: branch2_middle_procedure
        type(Procedure_t) :: branch3_bottom_procedure
        type(Procedure_t) :: branch3_middle_procedure
        type(Procedure_t) :: top_level_procedure

        branch1_bottom_module = Module_("Branch1_originating_m")
        branch1_middle_module = Module_("Branch1_middle_m")
        branch2_bottom_module = Module_("Branch2_originating_m")
        branch2_middle_module = Module_("Branch2_middle_m")
        branch3_bottom_module = Module_("Branch3_originating_m")
        branch3_middle_module = Module_("Branch3_middle_m")
        top_level_module = Module_("Top_level_m")
        branch1_bottom_procedure = Procedure_("branch1Originating")
        branch1_middle_procedure = Procedure_("branch1Middle")
        branch2_bottom_procedure = Procedure_("branch2Originating")
        branch2_middle_procedure = Procedure_("branch2Middle")
        branch3_bottom_procedure = Procedure_("branch3Originating")
        branch3_middle_procedure = Procedure_("branch3Middle")
        top_level_procedure = Procedure_("topLevel")

        call branch1_bottom_errors%appendError(Fatal( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        call branch1_middle_errors%appendErrors( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_errors%appendError(Fatal( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        call branch2_middle_errors%appendErrors( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_errors%appendError(Fatal( &
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
                assertEquals( &
                        0, &
                        size(top_level_errors.comingThrough.branch1_bottom_procedure), &
                        branch1_bottom_procedure%repr()) &
                .and.assertEquals( &
                        1, &
                        size(top_level_errors.comingThrough.branch1_middle_procedure), &
                        branch1_middle_procedure%repr()) &
                .and.assertEquals( &
                        1, &
                        size(top_level_errors.comingThrough.branch2_middle_procedure), &
                        branch2_middle_procedure%repr()) &
                .and.assertEquals( &
                        2, &
                        size(top_level_errors.comingThrough.[branch1_middle_procedure, branch2_middle_procedure]), &
                        branch1_middle_procedure%repr() // " or " // branch2_middle_procedure%repr())
    end function checkFilterByProceduresThrough

    function checkFilterByModulesFrom() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Fatal
        use Error_list_m, only: ErrorList_t, size
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertEquals

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
        type(Procedure_t) :: branch1_bottom_procedure
        type(Procedure_t) :: branch1_middle_procedure
        type(Procedure_t) :: branch2_bottom_procedure
        type(Procedure_t) :: branch2_middle_procedure
        type(Procedure_t) :: branch3_bottom_procedure
        type(Procedure_t) :: branch3_middle_procedure
        type(Procedure_t) :: top_level_procedure

        branch1_bottom_module = Module_("Branch1_originating_m")
        branch1_middle_module = Module_("Branch1_middle_m")
        branch2_bottom_module = Module_("Branch2_originating_m")
        branch2_middle_module = Module_("Branch2_middle_m")
        branch3_bottom_module = Module_("Branch3_originating_m")
        branch3_middle_module = Module_("Branch3_middle_m")
        top_level_module = Module_("Top_level_m")
        branch1_bottom_procedure = Procedure_("branch1Originating")
        branch1_middle_procedure = Procedure_("branch1Middle")
        branch2_bottom_procedure = Procedure_("branch2Originating")
        branch2_middle_procedure = Procedure_("branch2Middle")
        branch3_bottom_procedure = Procedure_("branch3Originating")
        branch3_middle_procedure = Procedure_("branch3Middle")
        top_level_procedure = Procedure_("topLevel")

        call branch1_bottom_errors%appendError(Fatal( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        call branch1_middle_errors%appendErrors( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_errors%appendError(Fatal( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        call branch2_middle_errors%appendErrors( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_errors%appendError(Fatal( &
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
                assertEquals( &
                        1, &
                        size(top_level_errors.from.branch1_bottom_module), &
                        branch1_bottom_module%repr()) &
                .and.assertEquals( &
                        1, &
                        size(top_level_errors.from.branch1_middle_module), &
                        branch1_middle_module%repr()) &
                .and.assertEquals( &
                        1, &
                        size(top_level_errors.from.branch2_middle_module), &
                        branch2_middle_module%repr()) &
                .and.assertEquals( &
                        2, &
                        size(top_level_errors.from.[branch1_middle_module, branch2_middle_module]), &
                        branch1_middle_module%repr() // " or " // branch2_middle_module%repr())
    end function checkFilterByModulesFrom

    function checkFilterByProceduresFrom() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Fatal
        use Error_list_m, only: ErrorList_t, size
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertEquals

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
        type(Procedure_t) :: branch1_bottom_procedure
        type(Procedure_t) :: branch1_middle_procedure
        type(Procedure_t) :: branch2_bottom_procedure
        type(Procedure_t) :: branch2_middle_procedure
        type(Procedure_t) :: branch3_bottom_procedure
        type(Procedure_t) :: branch3_middle_procedure
        type(Procedure_t) :: top_level_procedure

        branch1_bottom_module = Module_("Branch1_originating_m")
        branch1_middle_module = Module_("Branch1_middle_m")
        branch2_bottom_module = Module_("Branch2_originating_m")
        branch2_middle_module = Module_("Branch2_middle_m")
        branch3_bottom_module = Module_("Branch3_originating_m")
        branch3_middle_module = Module_("Branch3_middle_m")
        top_level_module = Module_("Top_level_m")
        branch1_bottom_procedure = Procedure_("branch1Originating")
        branch1_middle_procedure = Procedure_("branch1Middle")
        branch2_bottom_procedure = Procedure_("branch2Originating")
        branch2_middle_procedure = Procedure_("branch2Middle")
        branch3_bottom_procedure = Procedure_("branch3Originating")
        branch3_middle_procedure = Procedure_("branch3Middle")
        top_level_procedure = Procedure_("topLevel")

        call branch1_bottom_errors%appendError(Fatal( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        call branch1_middle_errors%appendErrors( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_errors%appendError(Fatal( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        call branch2_middle_errors%appendErrors( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_errors%appendError(Fatal( &
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
                assertEquals( &
                        1, &
                        size(top_level_errors.from.branch1_bottom_procedure), &
                        branch1_bottom_procedure%repr()) &
                .and.assertEquals( &
                        1, &
                        size(top_level_errors.from.branch1_middle_procedure), &
                        branch1_middle_procedure%repr()) &
                .and.assertEquals( &
                        1, &
                        size(top_level_errors.from.branch2_middle_procedure), &
                        branch2_middle_procedure%repr()) &
                .and.assertEquals( &
                        2, &
                        size(top_level_errors.from.[branch1_middle_procedure, branch2_middle_procedure]), &
                        branch1_middle_procedure%repr() // " or " // branch2_middle_procedure%repr())
    end function checkFilterByProceduresFrom

    function checkFilterByContents() result(result_)
        use iso_varying_string, only: VARYING_STRING, assignment(=), operator(//)
        use Message_m, only: Fatal
        use Error_list_m, only: ErrorList_t, size
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(VARYING_STRING) :: test_string1
        type(VARYING_STRING) :: test_string2

        test_string1 = "Hello"
        test_string2 = "Test"

        call errors%appendError(Fatal( &
                Module_("Some_m"), Procedure_("some"), "Hello Test"))
        call errors%appendError(Fatal( &
                Module_("Some_m"), Procedure_("some"), "Goodbye Test"))
        call errors%appendError(Fatal( &
                Module_("Some_m"), Procedure_("some"), "Example Error"))
        call errors%appendError(Fatal( &
                Module_("Some_m"), Procedure_("some"), "Simple Error"))
        call errors%appendError(Fatal( &
                Module_("Some_m"), Procedure_("some"), "Test Error"))
        call errors%appendError(Fatal( &
                Module_("Some_m"), Procedure_("some"), "Hello Error"))

        result_ = &
                assertEquals( &
                        2, &
                        size(errors.including."Hello"), &
                        'including "Hello"') &
                .and.assertEquals( &
                        3, &
                        size(errors.including.test_string2), &
                        'including "' // test_string2 // '"') &
                .and.assertEquals( &
                        4, &
                        size(errors.includingAnyOf.[test_string1, test_string2]), &
                        'includingAnyOf "' // test_string1 // '" or "' // test_string2 // '"') &
                .and.assertEquals( &
                        1, &
                        size(errors.includingAllOf.[test_string1, test_string2]), &
                        'includingAllOf "' // test_string1 // '" or "' // test_string2 // '"')
    end function checkFilterByContents

    function checkForType() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Fatal, FATAL_TYPE
        use Error_list_m, only: ErrorList_t
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(ErrorList_t) :: empty_list
        type(ErrorList_t) :: errors

        call errors%appendError(Fatal( &
                Module_("Some_m"), Procedure_("some"), "Test Error"))

        result_ = &
                assertNot( &
                        empty_list.hasType.FATAL_TYPE, &
                        empty_list%repr() // ".hasType." // FATAL_TYPE%repr()) &
                .and.assertThat( &
                        errors.hasType.FATAL_TYPE, &
                        errors%repr() // ".hasType." // FATAL_TYPE%repr())
    end function checkForType

    function checkForOriginatingModule() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Fatal
        use Error_list_m, only: ErrorList_t
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(ErrorList_t) :: empty_list
        type(ErrorList_t) :: errors

        type(Module_t) :: module1
        type(Module_t) :: module2
        type(Procedure_t) :: procedure1
        type(Procedure_t) :: procedure2

        module1 = Module_("Some_m")
        module2 = Module_("Another_m")
        procedure1 = Procedure_("some")
        procedure2 = Procedure_("another")

        call errors%appendError(Fatal( &
                module1, procedure1, "Test error"))
        call errors%appendError(Fatal( &
                module2, procedure2, "Another error"))

        result_ = &
                assertNot( &
                        empty_list.hasAnyOriginatingFrom.module1, &
                        empty_list%repr() // ".hasAnyOriginatingFrom." // module1%repr()) &
                .and.assertThat( &
                        errors.hasAnyOriginatingFrom.module1, &
                        errors%repr() // ".hasAnyOriginatingFrom." // module1%repr())
    end function checkForOriginatingModule

    function checkForOriginatingProcedure() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Fatal
        use Error_list_m, only: ErrorList_t
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(ErrorList_t) :: empty_list
        type(ErrorList_t) :: errors

        type(Module_t) :: module1
        type(Module_t) :: module2
        type(Procedure_t) :: procedure1
        type(Procedure_t) :: procedure2

        module1 = Module_("Some_m")
        module2 = Module_("Another_m")
        procedure1 = Procedure_("some")
        procedure2 = Procedure_("another")

        call errors%appendError(Fatal( &
                module1, procedure1, "Test error"))
        call errors%appendError(Fatal( &
                module2, procedure2, "Another error"))

        result_ = &
                assertNot( &
                        empty_list.hasAnyOriginatingFrom.procedure1, &
                        empty_list%repr() // ".hasAnyOriginatingFrom." // procedure1%repr()) &
                .and.assertThat( &
                        errors.hasAnyOriginatingFrom.procedure1, &
                        errors%repr() // ".hasAnyOriginatingFrom." // procedure1%repr())
    end function checkForOriginatingProcedure

    function checkForThroughModule() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Fatal
        use Error_list_m, only: ErrorList_t, size
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

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
        type(Procedure_t) :: branch1_bottom_procedure
        type(Procedure_t) :: branch1_middle_procedure
        type(Procedure_t) :: branch2_bottom_procedure
        type(Procedure_t) :: branch2_middle_procedure
        type(Procedure_t) :: branch3_bottom_procedure
        type(Procedure_t) :: branch3_middle_procedure
        type(Procedure_t) :: top_level_procedure

        branch1_bottom_module = Module_("Branch1_originating_m")
        branch1_middle_module = Module_("Branch1_middle_m")
        branch2_bottom_module = Module_("Branch2_originating_m")
        branch2_middle_module = Module_("Branch2_middle_m")
        branch3_bottom_module = Module_("Branch3_originating_m")
        branch3_middle_module = Module_("Branch3_middle_m")
        top_level_module = Module_("Top_level_m")
        branch1_bottom_procedure = Procedure_("branch1Originating")
        branch1_middle_procedure = Procedure_("branch1Middle")
        branch2_bottom_procedure = Procedure_("branch2Originating")
        branch2_middle_procedure = Procedure_("branch2Middle")
        branch3_bottom_procedure = Procedure_("branch3Originating")
        branch3_middle_procedure = Procedure_("branch3Middle")
        top_level_procedure = Procedure_("topLevel")

        call branch1_bottom_errors%appendError(Fatal( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        call branch1_middle_errors%appendErrors( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_errors%appendError(Fatal( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        call branch2_middle_errors%appendErrors( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_errors%appendError(Fatal( &
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
                assertThat( &
                        top_level_errors.hasAnyComingThrough.branch1_middle_module, &
                        top_level_errors%repr() // ".hasAnyCominghThrough." // branch1_middle_module%repr()) &
                .and.assertNot( &
                        top_level_errors.hasAnyComingThrough.branch1_bottom_module, &
                        top_level_errors%repr() // ".hasAnyCominghThrough." // branch1_bottom_module%repr())
    end function checkForThroughModule

    function checkForThroughProcedure() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Fatal
        use Error_list_m, only: ErrorList_t, size
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

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
        type(Procedure_t) :: branch1_bottom_procedure
        type(Procedure_t) :: branch1_middle_procedure
        type(Procedure_t) :: branch2_bottom_procedure
        type(Procedure_t) :: branch2_middle_procedure
        type(Procedure_t) :: branch3_bottom_procedure
        type(Procedure_t) :: branch3_middle_procedure
        type(Procedure_t) :: top_level_procedure

        branch1_bottom_module = Module_("Branch1_originating_m")
        branch1_middle_module = Module_("Branch1_middle_m")
        branch2_bottom_module = Module_("Branch2_originating_m")
        branch2_middle_module = Module_("Branch2_middle_m")
        branch3_bottom_module = Module_("Branch3_originating_m")
        branch3_middle_module = Module_("Branch3_middle_m")
        top_level_module = Module_("Top_level_m")
        branch1_bottom_procedure = Procedure_("branch1Originating")
        branch1_middle_procedure = Procedure_("branch1Middle")
        branch2_bottom_procedure = Procedure_("branch2Originating")
        branch2_middle_procedure = Procedure_("branch2Middle")
        branch3_bottom_procedure = Procedure_("branch3Originating")
        branch3_middle_procedure = Procedure_("branch3Middle")
        top_level_procedure = Procedure_("topLevel")

        call branch1_bottom_errors%appendError(Fatal( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        call branch1_middle_errors%appendErrors( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_errors%appendError(Fatal( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        call branch2_middle_errors%appendErrors( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_errors%appendError(Fatal( &
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
                assertThat( &
                        top_level_errors.hasAnyComingThrough.branch1_middle_procedure, &
                        top_level_errors%repr() // ".hasAnyCominghThrough." // branch1_middle_procedure%repr()) &
                .and.assertNot( &
                        top_level_errors.hasAnyComingThrough.branch1_bottom_procedure, &
                        top_level_errors%repr() // ".hasAnyCominghThrough." // branch1_bottom_procedure%repr())
    end function checkForThroughProcedure

    function checkForFromModule() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Fatal
        use Error_list_m, only: ErrorList_t, size
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertThat

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
        type(Procedure_t) :: branch1_bottom_procedure
        type(Procedure_t) :: branch1_middle_procedure
        type(Procedure_t) :: branch2_bottom_procedure
        type(Procedure_t) :: branch2_middle_procedure
        type(Procedure_t) :: branch3_bottom_procedure
        type(Procedure_t) :: branch3_middle_procedure
        type(Procedure_t) :: top_level_procedure

        branch1_bottom_module = Module_("Branch1_originating_m")
        branch1_middle_module = Module_("Branch1_middle_m")
        branch2_bottom_module = Module_("Branch2_originating_m")
        branch2_middle_module = Module_("Branch2_middle_m")
        branch3_bottom_module = Module_("Branch3_originating_m")
        branch3_middle_module = Module_("Branch3_middle_m")
        top_level_module = Module_("Top_level_m")
        branch1_bottom_procedure = Procedure_("branch1Originating")
        branch1_middle_procedure = Procedure_("branch1Middle")
        branch2_bottom_procedure = Procedure_("branch2Originating")
        branch2_middle_procedure = Procedure_("branch2Middle")
        branch3_bottom_procedure = Procedure_("branch3Originating")
        branch3_middle_procedure = Procedure_("branch3Middle")
        top_level_procedure = Procedure_("topLevel")

        call branch1_bottom_errors%appendError(Fatal( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        call branch1_middle_errors%appendErrors( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_errors%appendError(Fatal( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        call branch2_middle_errors%appendErrors( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_errors%appendError(Fatal( &
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
                assertThat( &
                        top_level_errors.hasAnyFrom.branch1_middle_module, &
                        top_level_errors%repr() // ".hasAnyFrom." // branch1_middle_module%repr()) &
                .and.assertThat( &
                        top_level_errors.hasAnyFrom.branch1_bottom_module, &
                        top_level_errors%repr() // ".hasAnyFrom." // branch1_bottom_module%repr())
    end function checkForFromModule

    function checkForFromProcedure() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Fatal
        use Error_list_m, only: ErrorList_t, size
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertThat

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
        type(Procedure_t) :: branch1_bottom_procedure
        type(Procedure_t) :: branch1_middle_procedure
        type(Procedure_t) :: branch2_bottom_procedure
        type(Procedure_t) :: branch2_middle_procedure
        type(Procedure_t) :: branch3_bottom_procedure
        type(Procedure_t) :: branch3_middle_procedure
        type(Procedure_t) :: top_level_procedure

        branch1_bottom_module = Module_("Branch1_originating_m")
        branch1_middle_module = Module_("Branch1_middle_m")
        branch2_bottom_module = Module_("Branch2_originating_m")
        branch2_middle_module = Module_("Branch2_middle_m")
        branch3_bottom_module = Module_("Branch3_originating_m")
        branch3_middle_module = Module_("Branch3_middle_m")
        top_level_module = Module_("Top_level_m")
        branch1_bottom_procedure = Procedure_("branch1Originating")
        branch1_middle_procedure = Procedure_("branch1Middle")
        branch2_bottom_procedure = Procedure_("branch2Originating")
        branch2_middle_procedure = Procedure_("branch2Middle")
        branch3_bottom_procedure = Procedure_("branch3Originating")
        branch3_middle_procedure = Procedure_("branch3Middle")
        top_level_procedure = Procedure_("topLevel")

        call branch1_bottom_errors%appendError(Fatal( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "error"))
        call branch1_middle_errors%appendErrors( &
                branch1_bottom_errors, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_errors%appendError(Fatal( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "error"))
        call branch2_middle_errors%appendErrors( &
                branch2_bottom_errors, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_errors%appendError(Fatal( &
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
                assertThat( &
                        top_level_errors.hasAnyFrom.branch1_middle_procedure, &
                        top_level_errors%repr() // ".hasAnyFrom." // branch1_middle_procedure%repr()) &
                .and.assertThat( &
                        top_level_errors.hasAnyFrom.branch1_bottom_procedure, &
                        top_level_errors%repr() // ".hasAnyFrom." // branch1_bottom_procedure%repr())
    end function checkForFromProcedure

    function checkForContents() result(result_)
        use iso_varying_string, only: VARYING_STRING, assignment(=), operator(//)
        use Message_m, only: Fatal
        use Error_list_m, only: ErrorList_t, size
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(VARYING_STRING) :: test_string1
        type(VARYING_STRING) :: test_string2
        type(VARYING_STRING) :: test_string3

        test_string1 = "Hello"
        test_string2 = "Test"
        test_string3 = "Other"

        call errors%appendError(Fatal( &
                Module_("Some_m"), Procedure_("some"), "Hello Test"))
        call errors%appendError(Fatal( &
                Module_("Some_m"), Procedure_("some"), "Goodbye Test"))
        call errors%appendError(Fatal( &
                Module_("Some_m"), Procedure_("some"), "Example Error"))
        call errors%appendError(Fatal( &
                Module_("Some_m"), Procedure_("some"), "Simple Error"))
        call errors%appendError(Fatal( &
                Module_("Some_m"), Procedure_("some"), "Test Error"))
        call errors%appendError(Fatal( &
                Module_("Some_m"), Procedure_("some"), "Hello Error"))

        result_ = &
                assertThat( &
                        errors.hasAnyIncluding."Hello", &
                        errors%repr() // '.hasAnyIncluding."Hello"') &
                .and.assertNot( &
                        errors.hasAnyIncluding.test_string3, &
                        errors%repr() // '.hasAnyIncluding."' // test_string3 // '"') &
                .and.assertThat( &
                        errors.hasAnyIncludingAnyOf.[test_string1, test_string2], &
                        errors%repr() // '.hasAnyIncludingAnyOf."' // test_string1 // '" or "' // test_string2 // '"') &
                .and.assertThat( &
                        errors.hasAnyIncludingAllOf.[test_string1, test_string2], &
                        errors%repr() // '.hasAnyIncludingAllOf."' // test_string1 // '" or "' // test_string2 // '"')
    end function checkForContents
end module error_list_test

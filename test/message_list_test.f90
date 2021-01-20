module message_list_test
    use iso_varying_string, only: VARYING_STRING, assignment(=), operator(//)
    use Message_list_m, only: MessageList_t, size
    use erloff_fatal_m, only: fatal_t
    use erloff_info_m, only: info_t, INFO
    use erloff_message_m, only: Message_t
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: procedure_t
    use erloff_warning_m, only: warning_t, WARNING
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

    public :: test_message_list
contains
    function test_message_list() result(tests)
        type(Test_Item_t) :: tests

        type(Test_Item_t) :: individual_tests(22)

        individual_tests(1) = It( &
                "Converts to an empty string when it is empty", &
                checkEmptyToString)
        individual_tests(2) = It( &
                "Can append a message to an empty list", &
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
                "Can filter messages by type", checkFilterByType)
        individual_tests(8) = It( &
                "Can filter messages by the originating module", &
                checkFilterByOriginatingModule)
        individual_tests(9) = It( &
                "Can filter messages by the originating procedure", &
                checkFilterByOriginatingProcedure)
        individual_tests(10) = It( &
                "Can filter messages by modules passed through", &
                checkFilterByModulesThrough)
        individual_tests(11) = It( &
                "Can filter messages by procedures passed through", &
                checkFilterByProceduresThrough)
        individual_tests(12) = It( &
                "Can filter messages by the modules they are from", &
                checkFilterByModulesFrom)
        individual_tests(13) = It( &
                "Can filter messages by the procedures they are from", &
                checkFilterByProceduresFrom)
        individual_tests(14) = It( &
                "Can filter messages based on their contents", &
                checkFilterByContents)
        individual_tests(15) = It( &
                "Can tell if it has a message of a given type", &
                checkForType)
        individual_tests(16) = It( &
                "Can tell if it has a message originating from a module", &
                checkForOriginatingModule)
        individual_tests(17) = It( &
                "Can tell if it has a message originating from a procedure", &
                checkForOriginatingProcedure)
        individual_tests(18) = It( &
                "Can tell if it has a message coming through a module", &
                checkForThroughModule)
        individual_tests(19) = It( &
                "Can tell if it has a message coming through a procedure", &
                checkForThroughProcedure)
        individual_tests(20) = It( &
                "Can tell if it has a message coming from a module", &
                checkForFromModule)
        individual_tests(21) = It( &
                "Can tell if it has a message comming from a procedure", &
                checkForFromProcedure)
        individual_tests(22) = It( &
                "Can tell if it has a message with some contents", &
                checkForContents)
        tests = Describe("MessageList_t", individual_tests)
    end function test_message_list

    pure function checkEmptyToString() result(result_)
        type(Result_t) :: result_

        type(MessageList_t) :: message_list

        result_ = assert_Empty(message_list%toString())
    end function checkEmptyToString

    pure function checkAppendToEmpty() result(result_)
        type(Result_t) :: result_

        class(Message_t), allocatable :: message
        type(MessageList_t) :: message_list

        allocate(message, source = info_t( &
                module_t("Some_m"), procedure_t("some"), "Test Message"))
        call message_list%appendMessage(message)

        result_ = assert_Includes(message%to_string(), message_list%toString())
    end function checkAppendToEmpty

    pure function checkAppendMultipleToEmpty() result(result_)
        type(Result_t) :: result_

        class(Message_t), allocatable :: message1
        class(Message_t), allocatable :: message2
        type(MessageList_t) :: message_list1
        type(MessageList_t) :: message_list2

        allocate(message1, source = info_t( &
                module_t("Some_m"), procedure_t("some"), "First Message"))
        allocate(message2, source = warning_t( &
                module_t("Some_m"), procedure_t("some"), "Second Message"))
        call message_list1%appendMessage(message1)
        call message_list1%appendMessage(message2)

        call message_list2%appendMessages( &
                message_list1, module_t("Another_m"), procedure_t("another"))

        result_ = &
                assert_Includes(message1%to_string(), message_list2%toString()) &
                .and.assert_Includes(message2%to_string(), message_list2%toString())
    end function checkAppendMultipleToEmpty

    pure function checkAppendEmpty() result(result_)
        type(Result_t) :: result_

        class(Message_t), allocatable :: message1
        class(Message_t), allocatable :: message2
        type(MessageList_t) :: message_list1
        type(MessageList_t) :: message_list2

        allocate(message1, source = info_t( &
                module_t("Some_m"), procedure_t("some"), "First Message"))
        allocate(message2, source = warning_t( &
                module_t("Some_m"), procedure_t("some"), "Second Message"))
        call message_list1%appendMessage(message1)
        call message_list1%appendMessage(message2)

        call message_list1%appendMessages( &
                message_list2, module_t("Another_m"), procedure_t("another"))

        result_ = &
                assert_Includes(message1%to_string(), message_list1%toString()) &
                .and.assert_Includes(message2%to_string(), message_list1%toString())
    end function checkAppendEmpty

    pure function checkCombineEmpty() result(result_)
        type(Result_t) :: result_

        type(MessageList_t) :: message_list1
        type(MessageList_t) :: message_list2

        call message_list1%appendMessages( &
                message_list2, module_t("Another_m"), procedure_t("another"))

        result_ = assert_Empty(message_list1%toString())
    end function checkCombineEmpty

    pure function checkCombine() result(result_)
        type(Result_t) :: result_

        class(Message_t), allocatable :: message1
        class(Message_t), allocatable :: message2
        class(Message_t), allocatable :: message3
        class(Message_t), allocatable :: message4
        type(MessageList_t) :: message_list1
        type(MessageList_t) :: message_list2

        allocate(message1, source = info_t( &
                module_t("Some_m"), procedure_t("some"), "First Message"))
        allocate(message2, source = warning_t( &
                module_t("Some_m"), procedure_t("some"), "Second Message"))
        call message_list1%appendMessage(message1)
        call message_list1%appendMessage(message2)

        allocate(message3, source = info_t( &
                module_t("Some_m"), procedure_t("some"), "Third Message"))
        allocate(message4, source = warning_t( &
                module_t("Some_m"), procedure_t("some"), "Fourth Message"))
        call message_list2%appendMessage(message3)
        call message_list2%appendMessage(message4)

        call message_list1%appendMessages( &
                message_list2, module_t("Another_m"), procedure_t("another"))

        result_ = &
                assert_Includes(message1%to_string(), message_list1%toString()) &
                .and.assert_Includes(message2%to_string(), message_list1%toString()) &
                .and.assert_Includes(message3%to_string(), message_list1%toString()) &
                .and.assert_Includes(message4%to_string(), message_list1%toString())
    end function checkCombine

    pure function checkFilterByType() result(result_)
        type(Result_t) :: result_

        type(MessageList_t) :: messages

        call messages%appendMessage(info_t( &
                module_t("Some_m"), procedure_t("some"), "Test message"))
        call messages%appendMessage(warning_t( &
                module_t("Some_m"), procedure_t("some"), "Test warning"))
        call messages%appendMessage(fatal_t( &
                module_t("Some_m"), procedure_t("some"), "Test error"))

        result_ = &
                assert_Equals(1, size(messages.ofType.INFO), "INFO") &
                .and.assert_Equals( &
                        2, &
                        size(messages.ofTypes.[INFO, WARNING]), &
                        "INFO or WARNING")
    end function checkFilterByType

    pure function checkFilterByOriginatingModule() result(result_)
        type(Result_t) :: result_

        type(MessageList_t) :: messages
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

        call messages%appendMessage(info_t( &
                module1, procedure1, "Test message"))
        call messages%appendMessage(info_t( &
                module2, procedure2, "Another message"))
        call messages%appendMessage(info_t( &
                module3, procedure3, "Yet another message"))

        result_ = &
                assert_Equals( &
                        1, &
                        size(messages.originatingFrom.module1), &
                        module1%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(messages.originatingFrom.module2), &
                        module2%repr()) &
                .and.assert_Equals( &
                        2, &
                        size(messages.originatingFrom.[module1, module2]), &
                        module1%repr() // " or " // module2%repr())
    end function checkFilterByOriginatingModule

    pure function checkFilterByOriginatingProcedure() result(result_)
        type(Result_t) :: result_

        type(MessageList_t) :: messages
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

        call messages%appendMessage(info_t( &
                module1, procedure1, "Test message"))
        call messages%appendMessage(info_t( &
                module2, procedure2, "Another message"))
        call messages%appendMessage(info_t( &
                module3, procedure3, "Yet another message"))

        result_ = &
                assert_Equals( &
                        1, &
                        size(messages.originatingFrom.procedure1), &
                        procedure1%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(messages.originatingFrom.procedure2), &
                        procedure2%repr()) &
                .and.assert_Equals( &
                        2, &
                        size(messages.originatingFrom.[procedure1, procedure2]), &
                        procedure1%repr() // " or " // procedure2%repr())
    end function checkFilterByOriginatingProcedure

    pure function checkFilterByModulesThrough() result(result_)
        type(Result_t) :: result_

        type(MessageList_t) :: branch1_bottom_messages
        type(MessageList_t) :: branch1_middle_messages
        type(MessageList_t) :: branch2_bottom_messages
        type(MessageList_t) :: branch2_middle_messages
        type(MessageList_t) :: branch3_bottom_messages
        type(MessageList_t) :: branch3_middle_messages
        type(MessageList_t) :: top_level_messages
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

        call branch1_bottom_messages%appendMessage(info_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "message"))
        call branch1_middle_messages%appendMessages( &
                branch1_bottom_messages, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_messages%appendMessage(info_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "message"))
        call branch2_middle_messages%appendMessages( &
                branch2_bottom_messages, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_messages%appendMessage(info_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "message"))
        call branch3_middle_messages%appendMessages( &
                branch3_bottom_messages, &
                branch3_middle_module, &
                branch3_middle_procedure)
        call top_level_messages%appendMessages( &
                branch1_middle_messages, &
                top_level_module, &
                top_level_procedure)
        call top_level_messages%appendMessages( &
                branch2_middle_messages, &
                top_level_module, &
                top_level_procedure)
        call top_level_messages%appendMessages( &
                branch3_middle_messages, &
                top_level_module, &
                top_level_procedure)

        result_ = &
                assert_Equals( &
                        0, &
                        size(top_level_messages.comingThrough.branch1_bottom_module), &
                        branch1_bottom_module%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(top_level_messages.comingThrough.branch1_middle_module), &
                        branch1_middle_module%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(top_level_messages.comingThrough.branch2_middle_module), &
                        branch2_middle_module%repr()) &
                .and.assert_Equals( &
                        2, &
                        size(top_level_messages.comingThrough.[branch1_middle_module, branch2_middle_module]), &
                        branch1_middle_module%repr() // " or " // branch2_middle_module%repr())
    end function checkFilterByModulesThrough

    pure function checkFilterByProceduresThrough() result(result_)
        type(Result_t) :: result_

        type(MessageList_t) :: branch1_bottom_messages
        type(MessageList_t) :: branch1_middle_messages
        type(MessageList_t) :: branch2_bottom_messages
        type(MessageList_t) :: branch2_middle_messages
        type(MessageList_t) :: branch3_bottom_messages
        type(MessageList_t) :: branch3_middle_messages
        type(MessageList_t) :: top_level_messages
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

        call branch1_bottom_messages%appendMessage(info_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "message"))
        call branch1_middle_messages%appendMessages( &
                branch1_bottom_messages, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_messages%appendMessage(info_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "message"))
        call branch2_middle_messages%appendMessages( &
                branch2_bottom_messages, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_messages%appendMessage(info_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "message"))
        call branch3_middle_messages%appendMessages( &
                branch3_bottom_messages, &
                branch3_middle_module, &
                branch3_middle_procedure)
        call top_level_messages%appendMessages( &
                branch1_middle_messages, &
                top_level_module, &
                top_level_procedure)
        call top_level_messages%appendMessages( &
                branch2_middle_messages, &
                top_level_module, &
                top_level_procedure)
        call top_level_messages%appendMessages( &
                branch3_middle_messages, &
                top_level_module, &
                top_level_procedure)

        result_ = &
                assert_Equals( &
                        0, &
                        size(top_level_messages.comingThrough.branch1_bottom_procedure), &
                        branch1_bottom_procedure%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(top_level_messages.comingThrough.branch1_middle_procedure), &
                        branch1_middle_procedure%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(top_level_messages.comingThrough.branch2_middle_procedure), &
                        branch2_middle_procedure%repr()) &
                .and.assert_Equals( &
                        2, &
                        size(top_level_messages.comingThrough.[branch1_middle_procedure, branch2_middle_procedure]), &
                        branch1_middle_procedure%repr() // " or " // branch2_middle_procedure%repr())
    end function checkFilterByProceduresThrough

    pure function checkFilterByModulesFrom() result(result_)
        type(Result_t) :: result_

        type(MessageList_t) :: branch1_bottom_messages
        type(MessageList_t) :: branch1_middle_messages
        type(MessageList_t) :: branch2_bottom_messages
        type(MessageList_t) :: branch2_middle_messages
        type(MessageList_t) :: branch3_bottom_messages
        type(MessageList_t) :: branch3_middle_messages
        type(MessageList_t) :: top_level_messages
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

        call branch1_bottom_messages%appendMessage(info_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "message"))
        call branch1_middle_messages%appendMessages( &
                branch1_bottom_messages, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_messages%appendMessage(info_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "message"))
        call branch2_middle_messages%appendMessages( &
                branch2_bottom_messages, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_messages%appendMessage(info_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "message"))
        call branch3_middle_messages%appendMessages( &
                branch3_bottom_messages, &
                branch3_middle_module, &
                branch3_middle_procedure)
        call top_level_messages%appendMessages( &
                branch1_middle_messages, &
                top_level_module, &
                top_level_procedure)
        call top_level_messages%appendMessages( &
                branch2_middle_messages, &
                top_level_module, &
                top_level_procedure)
        call top_level_messages%appendMessages( &
                branch3_middle_messages, &
                top_level_module, &
                top_level_procedure)

        result_ = &
                assert_Equals( &
                        1, &
                        size(top_level_messages.from.branch1_bottom_module), &
                        branch1_bottom_module%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(top_level_messages.from.branch1_middle_module), &
                        branch1_middle_module%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(top_level_messages.from.branch2_middle_module), &
                        branch2_middle_module%repr()) &
                .and.assert_Equals( &
                        2, &
                        size(top_level_messages.from.[branch1_middle_module, branch2_middle_module]), &
                        branch1_middle_module%repr() // " or " // branch2_middle_module%repr())
    end function checkFilterByModulesFrom

    pure function checkFilterByProceduresFrom() result(result_)
        type(Result_t) :: result_

        type(MessageList_t) :: branch1_bottom_messages
        type(MessageList_t) :: branch1_middle_messages
        type(MessageList_t) :: branch2_bottom_messages
        type(MessageList_t) :: branch2_middle_messages
        type(MessageList_t) :: branch3_bottom_messages
        type(MessageList_t) :: branch3_middle_messages
        type(MessageList_t) :: top_level_messages
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

        call branch1_bottom_messages%appendMessage(info_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "message"))
        call branch1_middle_messages%appendMessages( &
                branch1_bottom_messages, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_messages%appendMessage(info_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "message"))
        call branch2_middle_messages%appendMessages( &
                branch2_bottom_messages, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_messages%appendMessage(info_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "message"))
        call branch3_middle_messages%appendMessages( &
                branch3_bottom_messages, &
                branch3_middle_module, &
                branch3_middle_procedure)
        call top_level_messages%appendMessages( &
                branch1_middle_messages, &
                top_level_module, &
                top_level_procedure)
        call top_level_messages%appendMessages( &
                branch2_middle_messages, &
                top_level_module, &
                top_level_procedure)
        call top_level_messages%appendMessages( &
                branch3_middle_messages, &
                top_level_module, &
                top_level_procedure)

        result_ = &
                assert_Equals( &
                        1, &
                        size(top_level_messages.from.branch1_bottom_procedure), &
                        branch1_bottom_procedure%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(top_level_messages.from.branch1_middle_procedure), &
                        branch1_middle_procedure%repr()) &
                .and.assert_Equals( &
                        1, &
                        size(top_level_messages.from.branch2_middle_procedure), &
                        branch2_middle_procedure%repr()) &
                .and.assert_Equals( &
                        2, &
                        size(top_level_messages.from.[branch1_middle_procedure, branch2_middle_procedure]), &
                        branch1_middle_procedure%repr() // " or " // branch2_middle_procedure%repr())
    end function checkFilterByProceduresFrom

    pure function checkFilterByContents() result(result_)
        type(Result_t) :: result_

        type(MessageList_t) :: messages
        type(VARYING_STRING) :: test_string1
        type(VARYING_STRING) :: test_string2

        test_string1 = "Hello"
        test_string2 = "Test"

        call messages%appendMessage(info_t( &
                module_t("Some_m"), procedure_t("some"), "Hello Test"))
        call messages%appendMessage(info_t( &
                module_t("Some_m"), procedure_t("some"), "Goodbye Test"))
        call messages%appendMessage(info_t( &
                module_t("Some_m"), procedure_t("some"), "Example Message"))
        call messages%appendMessage(info_t( &
                module_t("Some_m"), procedure_t("some"), "Simple Message"))
        call messages%appendMessage(info_t( &
                module_t("Some_m"), procedure_t("some"), "Test Message"))
        call messages%appendMessage(info_t( &
                module_t("Some_m"), procedure_t("some"), "Hello Message"))

        result_ = &
                assert_Equals( &
                        2, &
                        size(messages.including."Hello"), &
                        'including "Hello"') &
                .and.assert_Equals( &
                        3, &
                        size(messages.including.test_string2), &
                        'including "' // test_string2 // '"') &
                .and.assert_Equals( &
                        4, &
                        size(messages.includingAnyOf.[test_string1, test_string2]), &
                        'includingAnyOf "' // test_string1 // '" or "' // test_string2 // '"') &
                .and.assert_Equals( &
                        1, &
                        size(messages.includingAllOf.[test_string1, test_string2]), &
                        'includingAllOf "' // test_string1 // '" or "' // test_string2 // '"')
    end function checkFilterByContents

    pure function checkForType() result(result_)
        type(Result_t) :: result_

        type(MessageList_t) :: empty_list
        type(MessageList_t) :: messages

        call messages%appendMessage(info_t( &
                module_t("Some_m"), procedure_t("some"), "Test Message"))

        result_ = &
                assert_Not( &
                        empty_list.hasType.INFO, &
                        empty_list%repr() // ".hasType." // INFO%repr()) &
                .and.assert_That( &
                        messages.hasType.INFO, &
                        messages%repr() // ".hasType." // INFO%repr())
    end function checkForType

    pure function checkForOriginatingModule() result(result_)
        type(Result_t) :: result_

        type(MessageList_t) :: empty_list
        type(MessageList_t) :: messages

        type(Module_t) :: module1
        type(Module_t) :: module2
        type(procedure_t) :: procedure1
        type(procedure_t) :: procedure2

        module1 = module_t("Some_m")
        module2 = module_t("Another_m")
        procedure1 = procedure_t("some")
        procedure2 = procedure_t("another")

        call messages%appendMessage(info_t( &
                module1, procedure1, "Test message"))
        call messages%appendMessage(info_t( &
                module2, procedure2, "Another message"))

        result_ = &
                assert_Not( &
                        empty_list.hasAnyOriginatingFrom.module1, &
                        empty_list%repr() // ".hasAnyOriginatingFrom." // module1%repr()) &
                .and.assert_That( &
                        messages.hasAnyOriginatingFrom.module1, &
                        messages%repr() // ".hasAnyOriginatingFrom." // module1%repr())
    end function checkForOriginatingModule

    pure function checkForOriginatingProcedure() result(result_)
        type(Result_t) :: result_

        type(MessageList_t) :: empty_list
        type(MessageList_t) :: messages

        type(Module_t) :: module1
        type(Module_t) :: module2
        type(procedure_t) :: procedure1
        type(procedure_t) :: procedure2

        module1 = module_t("Some_m")
        module2 = module_t("Another_m")
        procedure1 = procedure_t("some")
        procedure2 = procedure_t("another")

        call messages%appendMessage(info_t( &
                module1, procedure1, "Test message"))
        call messages%appendMessage(info_t( &
                module2, procedure2, "Another message"))

        result_ = &
                assert_Not( &
                        empty_list.hasAnyOriginatingFrom.procedure1, &
                        empty_list%repr() // ".hasAnyOriginatingFrom." // procedure1%repr()) &
                .and.assert_That( &
                        messages.hasAnyOriginatingFrom.procedure1, &
                        messages%repr() // ".hasAnyOriginatingFrom." // procedure1%repr())
    end function checkForOriginatingProcedure

    pure function checkForThroughModule() result(result_)
        type(Result_t) :: result_

        type(MessageList_t) :: branch1_bottom_messages
        type(MessageList_t) :: branch1_middle_messages
        type(MessageList_t) :: branch2_bottom_messages
        type(MessageList_t) :: branch2_middle_messages
        type(MessageList_t) :: branch3_bottom_messages
        type(MessageList_t) :: branch3_middle_messages
        type(MessageList_t) :: top_level_messages
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

        call branch1_bottom_messages%appendMessage(info_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "message"))
        call branch1_middle_messages%appendMessages( &
                branch1_bottom_messages, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_messages%appendMessage(info_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "message"))
        call branch2_middle_messages%appendMessages( &
                branch2_bottom_messages, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_messages%appendMessage(info_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "message"))
        call branch3_middle_messages%appendMessages( &
                branch3_bottom_messages, &
                branch3_middle_module, &
                branch3_middle_procedure)
        call top_level_messages%appendMessages( &
                branch1_middle_messages, &
                top_level_module, &
                top_level_procedure)
        call top_level_messages%appendMessages( &
                branch2_middle_messages, &
                top_level_module, &
                top_level_procedure)
        call top_level_messages%appendMessages( &
                branch3_middle_messages, &
                top_level_module, &
                top_level_procedure)

        result_ = &
                assert_That( &
                        top_level_messages.hasAnyComingThrough.branch1_middle_module, &
                        top_level_messages%repr() // ".hasAnyCominghThrough." // branch1_middle_module%repr()) &
                .and.assert_Not( &
                        top_level_messages.hasAnyComingThrough.branch1_bottom_module, &
                        top_level_messages%repr() // ".hasAnyCominghThrough." // branch1_bottom_module%repr())
    end function checkForThroughModule

    pure function checkForThroughProcedure() result(result_)
        type(Result_t) :: result_

        type(MessageList_t) :: branch1_bottom_messages
        type(MessageList_t) :: branch1_middle_messages
        type(MessageList_t) :: branch2_bottom_messages
        type(MessageList_t) :: branch2_middle_messages
        type(MessageList_t) :: branch3_bottom_messages
        type(MessageList_t) :: branch3_middle_messages
        type(MessageList_t) :: top_level_messages
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

        call branch1_bottom_messages%appendMessage(info_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "message"))
        call branch1_middle_messages%appendMessages( &
                branch1_bottom_messages, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_messages%appendMessage(info_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "message"))
        call branch2_middle_messages%appendMessages( &
                branch2_bottom_messages, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_messages%appendMessage(info_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "message"))
        call branch3_middle_messages%appendMessages( &
                branch3_bottom_messages, &
                branch3_middle_module, &
                branch3_middle_procedure)
        call top_level_messages%appendMessages( &
                branch1_middle_messages, &
                top_level_module, &
                top_level_procedure)
        call top_level_messages%appendMessages( &
                branch2_middle_messages, &
                top_level_module, &
                top_level_procedure)
        call top_level_messages%appendMessages( &
                branch3_middle_messages, &
                top_level_module, &
                top_level_procedure)

        result_ = &
                assert_That( &
                        top_level_messages.hasAnyComingThrough.branch1_middle_procedure, &
                        top_level_messages%repr() // ".hasAnyCominghThrough." // branch1_middle_procedure%repr()) &
                .and.assert_Not( &
                        top_level_messages.hasAnyComingThrough.branch1_bottom_procedure, &
                        top_level_messages%repr() // ".hasAnyCominghThrough." // branch1_bottom_procedure%repr())
    end function checkForThroughProcedure

    pure function checkForFromModule() result(result_)
        type(Result_t) :: result_

        type(MessageList_t) :: branch1_bottom_messages
        type(MessageList_t) :: branch1_middle_messages
        type(MessageList_t) :: branch2_bottom_messages
        type(MessageList_t) :: branch2_middle_messages
        type(MessageList_t) :: branch3_bottom_messages
        type(MessageList_t) :: branch3_middle_messages
        type(MessageList_t) :: top_level_messages
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

        call branch1_bottom_messages%appendMessage(info_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "message"))
        call branch1_middle_messages%appendMessages( &
                branch1_bottom_messages, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_messages%appendMessage(info_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "message"))
        call branch2_middle_messages%appendMessages( &
                branch2_bottom_messages, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_messages%appendMessage(info_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "message"))
        call branch3_middle_messages%appendMessages( &
                branch3_bottom_messages, &
                branch3_middle_module, &
                branch3_middle_procedure)
        call top_level_messages%appendMessages( &
                branch1_middle_messages, &
                top_level_module, &
                top_level_procedure)
        call top_level_messages%appendMessages( &
                branch2_middle_messages, &
                top_level_module, &
                top_level_procedure)
        call top_level_messages%appendMessages( &
                branch3_middle_messages, &
                top_level_module, &
                top_level_procedure)

        result_ = &
                assert_That( &
                        top_level_messages.hasAnyFrom.branch1_middle_module, &
                        top_level_messages%repr() // ".hasAnyFrom." // branch1_middle_module%repr()) &
                .and.assert_That( &
                        top_level_messages.hasAnyFrom.branch1_bottom_module, &
                        top_level_messages%repr() // ".hasAnyFrom." // branch1_bottom_module%repr())
    end function checkForFromModule

    pure function checkForFromProcedure() result(result_)
        type(Result_t) :: result_

        type(MessageList_t) :: branch1_bottom_messages
        type(MessageList_t) :: branch1_middle_messages
        type(MessageList_t) :: branch2_bottom_messages
        type(MessageList_t) :: branch2_middle_messages
        type(MessageList_t) :: branch3_bottom_messages
        type(MessageList_t) :: branch3_middle_messages
        type(MessageList_t) :: top_level_messages
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

        call branch1_bottom_messages%appendMessage(info_t( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "message"))
        call branch1_middle_messages%appendMessages( &
                branch1_bottom_messages, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_messages%appendMessage(info_t( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "message"))
        call branch2_middle_messages%appendMessages( &
                branch2_bottom_messages, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_messages%appendMessage(info_t( &
                branch3_bottom_module, &
                branch3_bottom_procedure, &
                "message"))
        call branch3_middle_messages%appendMessages( &
                branch3_bottom_messages, &
                branch3_middle_module, &
                branch3_middle_procedure)
        call top_level_messages%appendMessages( &
                branch1_middle_messages, &
                top_level_module, &
                top_level_procedure)
        call top_level_messages%appendMessages( &
                branch2_middle_messages, &
                top_level_module, &
                top_level_procedure)
        call top_level_messages%appendMessages( &
                branch3_middle_messages, &
                top_level_module, &
                top_level_procedure)

        result_ = &
                assert_That( &
                        top_level_messages.hasAnyFrom.branch1_middle_procedure, &
                        top_level_messages%repr() // ".hasAnyFrom." // branch1_middle_procedure%repr()) &
                .and.assert_That( &
                        top_level_messages.hasAnyFrom.branch1_bottom_procedure, &
                        top_level_messages%repr() // ".hasAnyFrom." // branch1_bottom_procedure%repr())
    end function checkForFromProcedure

    pure function checkForContents() result(result_)
        type(Result_t) :: result_

        type(MessageList_t) :: messages
        type(VARYING_STRING) :: test_string1
        type(VARYING_STRING) :: test_string2
        type(VARYING_STRING) :: test_string3

        test_string1 = "Hello"
        test_string2 = "Test"
        test_string3 = "Other"

        call messages%appendMessage(info_t( &
                module_t("Some_m"), procedure_t("some"), "Hello Test"))
        call messages%appendMessage(info_t( &
                module_t("Some_m"), procedure_t("some"), "Goodbye Test"))
        call messages%appendMessage(info_t( &
                module_t("Some_m"), procedure_t("some"), "Example Message"))
        call messages%appendMessage(info_t( &
                module_t("Some_m"), procedure_t("some"), "Simple Message"))
        call messages%appendMessage(info_t( &
                module_t("Some_m"), procedure_t("some"), "Test Message"))
        call messages%appendMessage(info_t( &
                module_t("Some_m"), procedure_t("some"), "Hello Message"))

        result_ = &
                assert_That( &
                        messages.hasAnyIncluding."Hello", &
                        messages%repr() // '.hasAnyIncluding."Hello"') &
                .and.assert_Not( &
                        messages.hasAnyIncluding.test_string3, &
                        messages%repr() // '.hasAnyIncluding."' // test_string3 // '"') &
                .and.assert_That( &
                        messages.hasAnyIncludingAnyOf.[test_string1, test_string2], &
                        messages%repr() // '.hasAnyIncludingAnyOf."' // test_string1 // '" or "' // test_string2 // '"') &
                .and.assert_That( &
                        messages.hasAnyIncludingAllOf.[test_string1, test_string2], &
                        messages%repr() // '.hasAnyIncludingAllOf."' // test_string1 // '" or "' // test_string2 // '"')
    end function checkForContents
end module message_list_test

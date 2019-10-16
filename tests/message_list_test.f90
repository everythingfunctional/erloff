module message_list_test
    implicit none
    private

    public :: test_message_list
contains
    function test_message_list() result(tests)
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(15)

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
        tests = Describe("MessageList_t", individual_tests)
    end function test_message_list

    function checkEmptyToString() result(result_)
        use Message_list_m, only: MessageList_t
        use Vegetables_m, only: Result_t, assertEmpty

        type(Result_t) :: result_

        type(MessageList_t) :: message_list

        result_ = assertEmpty(message_list%toString())
    end function checkEmptyToString

    function checkAppendToEmpty() result(result_)
        use Message_m, only: Message_t, Info
        use Message_list_m, only: MessageList_t
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use Vegetables_m, only: Result_t, assertIncludes

        type(Result_t) :: result_

        class(Message_t), allocatable :: message
        type(MessageList_t) :: message_list

        allocate(message, source = Info( &
                Module_("Some_m"), Procedure_("some"), "Test Message"))
        call message_list%appendMessage(message)

        result_ = assertIncludes(message%toString(), message_list%toString())
    end function checkAppendToEmpty

    function checkAppendMultipleToEmpty() result(result_)
        use Message_m, only: Message_t, Info, Warning
        use Message_list_m, only: MessageList_t
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use Vegetables_m, only: Result_t, assertIncludes

        type(Result_t) :: result_

        class(Message_t), allocatable :: message1
        class(Message_t), allocatable :: message2
        type(MessageList_t) :: message_list1
        type(MessageList_t) :: message_list2

        allocate(message1, source = Info( &
                Module_("Some_m"), Procedure_("some"), "First Message"))
        allocate(message2, source = Warning( &
                Module_("Some_m"), Procedure_("some"), "Second Message"))
        call message_list1%appendMessage(message1)
        call message_list1%appendMessage(message2)

        call message_list2%appendMessages( &
                message_list1, Module_("Another_m"), Procedure_("another"))

        result_ = &
                assertIncludes(message1%toString(), message_list2%toString()) &
                .and.assertIncludes(message2%toString(), message_list2%toString())
    end function checkAppendMultipleToEmpty

    function checkAppendEmpty() result(result_)
        use Message_m, only: Message_t, Info, Warning
        use Message_list_m, only: MessageList_t
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use Vegetables_m, only: Result_t, assertIncludes

        type(Result_t) :: result_

        class(Message_t), allocatable :: message1
        class(Message_t), allocatable :: message2
        type(MessageList_t) :: message_list1
        type(MessageList_t) :: message_list2

        allocate(message1, source = Info( &
                Module_("Some_m"), Procedure_("some"), "First Message"))
        allocate(message2, source = Warning( &
                Module_("Some_m"), Procedure_("some"), "Second Message"))
        call message_list1%appendMessage(message1)
        call message_list1%appendMessage(message2)

        call message_list1%appendMessages( &
                message_list2, Module_("Another_m"), Procedure_("another"))

        result_ = &
                assertIncludes(message1%toString(), message_list1%toString()) &
                .and.assertIncludes(message2%toString(), message_list1%toString())
    end function checkAppendEmpty

    function checkCombineEmpty() result(result_)
        use Message_list_m, only: MessageList_t
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use Vegetables_m, only: Result_t, assertEmpty

        type(Result_t) :: result_

        type(MessageList_t) :: message_list1
        type(MessageList_t) :: message_list2

        call message_list1%appendMessages( &
                message_list2, Module_("Another_m"), Procedure_("another"))

        result_ = assertEmpty(message_list1%toString())
    end function checkCombineEmpty

    function checkCombine() result(result_)
        use Message_m, only: Message_t, Info, Warning
        use Message_list_m, only: MessageList_t
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use Vegetables_m, only: Result_t, assertIncludes

        type(Result_t) :: result_

        class(Message_t), allocatable :: message1
        class(Message_t), allocatable :: message2
        class(Message_t), allocatable :: message3
        class(Message_t), allocatable :: message4
        type(MessageList_t) :: message_list1
        type(MessageList_t) :: message_list2

        allocate(message1, source = Info( &
                Module_("Some_m"), Procedure_("some"), "First Message"))
        allocate(message2, source = Warning( &
                Module_("Some_m"), Procedure_("some"), "Second Message"))
        call message_list1%appendMessage(message1)
        call message_list1%appendMessage(message2)

        allocate(message3, source = Info( &
                Module_("Some_m"), Procedure_("some"), "Third Message"))
        allocate(message4, source = Warning( &
                Module_("Some_m"), Procedure_("some"), "Fourth Message"))
        call message_list2%appendMessage(message3)
        call message_list2%appendMessage(message4)

        call message_list1%appendMessages( &
                message_list2, Module_("Another_m"), Procedure_("another"))

        result_ = &
                assertIncludes(message1%toString(), message_list1%toString()) &
                .and.assertIncludes(message2%toString(), message_list1%toString()) &
                .and.assertIncludes(message3%toString(), message_list1%toString()) &
                .and.assertIncludes(message4%toString(), message_list1%toString())
    end function checkCombine

    function checkFilterByType() result(result_)
        use Message_m, only: Fatal, Info, Warning, INFO_TYPE, WARNING_TYPE
        use Message_list_m, only: MessageList_t, size
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(MessageList_t) :: messages

        call messages%appendMessage(Info( &
                Module_("Some_m"), Procedure_("some"), "Test message"))
        call messages%appendMessage(Warning( &
                Module_("Some_m"), Procedure_("some"), "Test warning"))
        call messages%appendMessage(Fatal( &
                Module_("Some_m"), Procedure_("some"), "Test error"))

        result_ = &
                assertEquals(1, size(messages.ofType.INFO_TYPE), "INFO") &
                .and.assertEquals( &
                        2, &
                        size(messages.ofTypes.[INFO_TYPE, WARNING_TYPE]), &
                        "INFO or WARNING")
    end function checkFilterByType

    function checkFilterByOriginatingModule() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Info
        use Message_list_m, only: MessageList_t, size
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(MessageList_t) :: messages
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

        call messages%appendMessage(Info( &
                module1, procedure1, "Test message"))
        call messages%appendMessage(Info( &
                module2, procedure2, "Another message"))
        call messages%appendMessage(Info( &
                module3, procedure3, "Yet another message"))

        result_ = &
                assertEquals( &
                        1, &
                        size(messages.originatingFrom.module1), &
                        module1%repr()) &
                .and.assertEquals( &
                        1, &
                        size(messages.originatingFrom.module2), &
                        module2%repr()) &
                .and.assertEquals( &
                        2, &
                        size(messages.originatingFrom.[module1, module2]), &
                        module1%repr() // " or " // module2%repr())
    end function checkFilterByOriginatingModule

    function checkFilterByOriginatingProcedure() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Info
        use Message_list_m, only: MessageList_t, size
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(MessageList_t) :: messages
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

        call messages%appendMessage(Info( &
                module1, procedure1, "Test message"))
        call messages%appendMessage(Info( &
                module2, procedure2, "Another message"))
        call messages%appendMessage(Info( &
                module3, procedure3, "Yet another message"))

        result_ = &
                assertEquals( &
                        1, &
                        size(messages.originatingFrom.procedure1), &
                        procedure1%repr()) &
                .and.assertEquals( &
                        1, &
                        size(messages.originatingFrom.procedure2), &
                        procedure2%repr()) &
                .and.assertEquals( &
                        2, &
                        size(messages.originatingFrom.[procedure1, procedure2]), &
                        procedure1%repr() // " or " // procedure2%repr())
    end function checkFilterByOriginatingProcedure

    function checkFilterByModulesThrough() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Info
        use Message_list_m, only: MessageList_t, size
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertEquals

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

        call branch1_bottom_messages%appendMessage(Info( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "message"))
        call branch1_middle_messages%appendMessages( &
                branch1_bottom_messages, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_messages%appendMessage(Info( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "message"))
        call branch2_middle_messages%appendMessages( &
                branch2_bottom_messages, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_messages%appendMessage(Info( &
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
                assertEquals( &
                        0, &
                        size(top_level_messages.comingThrough.branch1_bottom_module), &
                        branch1_bottom_module%repr()) &
                .and.assertEquals( &
                        1, &
                        size(top_level_messages.comingThrough.branch1_middle_module), &
                        branch1_middle_module%repr()) &
                .and.assertEquals( &
                        1, &
                        size(top_level_messages.comingThrough.branch2_middle_module), &
                        branch2_middle_module%repr()) &
                .and.assertEquals( &
                        2, &
                        size(top_level_messages.comingThrough.[branch1_middle_module, branch2_middle_module]), &
                        branch1_middle_module%repr() // " or " // branch2_middle_module%repr())
    end function checkFilterByModulesThrough

    function checkFilterByProceduresThrough() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Info
        use Message_list_m, only: MessageList_t, size
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertEquals

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

        call branch1_bottom_messages%appendMessage(Info( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "message"))
        call branch1_middle_messages%appendMessages( &
                branch1_bottom_messages, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_messages%appendMessage(Info( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "message"))
        call branch2_middle_messages%appendMessages( &
                branch2_bottom_messages, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_messages%appendMessage(Info( &
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
                assertEquals( &
                        0, &
                        size(top_level_messages.comingThrough.branch1_bottom_procedure), &
                        branch1_bottom_procedure%repr()) &
                .and.assertEquals( &
                        1, &
                        size(top_level_messages.comingThrough.branch1_middle_procedure), &
                        branch1_middle_procedure%repr()) &
                .and.assertEquals( &
                        1, &
                        size(top_level_messages.comingThrough.branch2_middle_procedure), &
                        branch2_middle_procedure%repr()) &
                .and.assertEquals( &
                        2, &
                        size(top_level_messages.comingThrough.[branch1_middle_procedure, branch2_middle_procedure]), &
                        branch1_middle_procedure%repr() // " or " // branch2_middle_procedure%repr())
    end function checkFilterByProceduresThrough

    function checkFilterByModulesFrom() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Info
        use Message_list_m, only: MessageList_t, size
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertEquals

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

        call branch1_bottom_messages%appendMessage(Info( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "message"))
        call branch1_middle_messages%appendMessages( &
                branch1_bottom_messages, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_messages%appendMessage(Info( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "message"))
        call branch2_middle_messages%appendMessages( &
                branch2_bottom_messages, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_messages%appendMessage(Info( &
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
                assertEquals( &
                        1, &
                        size(top_level_messages.from.branch1_bottom_module), &
                        branch1_bottom_module%repr()) &
                .and.assertEquals( &
                        1, &
                        size(top_level_messages.from.branch1_middle_module), &
                        branch1_middle_module%repr()) &
                .and.assertEquals( &
                        1, &
                        size(top_level_messages.from.branch2_middle_module), &
                        branch2_middle_module%repr()) &
                .and.assertEquals( &
                        2, &
                        size(top_level_messages.from.[branch1_middle_module, branch2_middle_module]), &
                        branch1_middle_module%repr() // " or " // branch2_middle_module%repr())
    end function checkFilterByModulesFrom

    function checkFilterByProceduresFrom() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Info
        use Message_list_m, only: MessageList_t, size
        use Module_m, only: Module_t, Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertEquals

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

        call branch1_bottom_messages%appendMessage(Info( &
                branch1_bottom_module, &
                branch1_bottom_procedure, &
                "message"))
        call branch1_middle_messages%appendMessages( &
                branch1_bottom_messages, &
                branch1_middle_module, &
                branch1_middle_procedure)
        call branch2_bottom_messages%appendMessage(Info( &
                branch2_bottom_module, &
                branch2_bottom_procedure, &
                "message"))
        call branch2_middle_messages%appendMessages( &
                branch2_bottom_messages, &
                branch2_middle_module, &
                branch2_middle_procedure)
        call branch3_bottom_messages%appendMessage(Info( &
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
                assertEquals( &
                        1, &
                        size(top_level_messages.from.branch1_bottom_procedure), &
                        branch1_bottom_procedure%repr()) &
                .and.assertEquals( &
                        1, &
                        size(top_level_messages.from.branch1_middle_procedure), &
                        branch1_middle_procedure%repr()) &
                .and.assertEquals( &
                        1, &
                        size(top_level_messages.from.branch2_middle_procedure), &
                        branch2_middle_procedure%repr()) &
                .and.assertEquals( &
                        2, &
                        size(top_level_messages.from.[branch1_middle_procedure, branch2_middle_procedure]), &
                        branch1_middle_procedure%repr() // " or " // branch2_middle_procedure%repr())
    end function checkFilterByProceduresFrom

    function checkFilterByContents() result(result_)
        use iso_varying_string, only: VARYING_STRING, assignment(=), operator(//)
        use Message_m, only: Info
        use Message_list_m, only: MessageList_t, size
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(MessageList_t) :: messages
        type(VARYING_STRING) :: test_string1
        type(VARYING_STRING) :: test_string2

        test_string1 = "Hello"
        test_string2 = "Test"

        call messages%appendMessage(Info( &
                Module_("Some_m"), Procedure_("some"), "Hello Test"))
        call messages%appendMessage(Info( &
                Module_("Some_m"), Procedure_("some"), "Goodbye Test"))
        call messages%appendMessage(Info( &
                Module_("Some_m"), Procedure_("some"), "Example Message"))
        call messages%appendMessage(Info( &
                Module_("Some_m"), Procedure_("some"), "Simple Message"))
        call messages%appendMessage(Info( &
                Module_("Some_m"), Procedure_("some"), "Test Message"))
        call messages%appendMessage(Info( &
                Module_("Some_m"), Procedure_("some"), "Hello Message"))

        result_ = &
                assertEquals( &
                        2, &
                        size(messages.including."Hello"), &
                        'containing "Hello"') &
                .and.assertEquals( &
                        3, &
                        size(messages.including.test_string2), &
                        'containing "' // test_string2 // '"') &
                .and.assertEquals( &
                        4, &
                        size(messages.includingAnyOf.[test_string1, test_string2]), &
                        'containingAnyOf "' // test_string1 // '" or "' // test_string2 // '"') &
                .and.assertEquals( &
                        1, &
                        size(messages.includingAllOf.[test_string1, test_string2]), &
                        'containingAllOf "' // test_string1 // '" or "' // test_string2 // '"')
    end function checkFilterByContents

    function checkForType() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Info, INFO_TYPE
        use Message_list_m, only: MessageList_t
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        type(MessageList_t) :: empty_list
        type(MessageList_t) :: messages

        call messages%appendMessage(Info( &
                Module_("Some_m"), Procedure_("some"), "Test Message"))

        result_ = &
                assertNot( &
                        empty_list.hasType.INFO_TYPE, &
                        empty_list%repr() // ".hasType." // INFO_TYPE%repr()) &
                .and.assertThat( &
                        messages.hasType.INFO_TYPE, &
                        messages%repr() // ".hasType." // INFO_TYPE%repr())
    end function checkForType
end module message_list_test

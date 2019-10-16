module message_list_test
    implicit none
    private

    public :: test_message_list
contains
    function test_message_list() result(tests)
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(9)

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
        use Procedure_m, only: Procedure_
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(MessageList_t) :: messages
        type(Module_t) :: module1
        type(Module_t) :: module2
        type(Module_t) :: module3

        module1 = Module_("Some_m")
        module2 = Module_("Another_m")
        module3 = Module_("Yet_another_m")

        call messages%appendMessage(Info( &
                module1, Procedure_("some"), "Test message"))
        call messages%appendMessage(Info( &
                module2, Procedure_("another"), "Another message"))
        call messages%appendMessage(Info( &
                module3, Procedure_("yetAnother"), "Yet another message"))

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
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_t, Procedure_
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(MessageList_t) :: messages
        type(Procedure_t) :: procedure1
        type(Procedure_t) :: procedure2
        type(Procedure_t) :: procedure3

        procedure1 = Procedure_("some")
        procedure2 = Procedure_("another")
        procedure3 = Procedure_("yetAnother")

        call messages%appendMessage(Info( &
                Module_("Some_m"), procedure1, "Test message"))
        call messages%appendMessage(Info( &
                Module_("Another_m"), procedure2, "Another message"))
        call messages%appendMessage(Info( &
                Module_("Yet_another_m"), procedure3, "Yet another message"))

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
end module message_list_test

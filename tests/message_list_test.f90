module message_list_test
    implicit none
    private

    public :: test_message_list
contains
    function test_message_list() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(8)

        individual_tests(1) = it( &
                "converts to an empty string when it is empty", &
                checkEmptyToString)
        individual_tests(2) = it( &
                "can append to an empty list", checkAppendToEmpty)
        individual_tests(3) = it( &
                "can append multiple to an empty list", &
                checkAppendMultipleToEmpty)
        individual_tests(4) = it( &
                "can append an empty list", checkAppendEmpty)
        individual_tests(5) = it( &
                "can combine two empty lists", checkCombineEmpty)
        individual_tests(6) = it( &
                "can combine two lists", checkCombine)
        individual_tests(7) = it( &
                "can prepend names to a list", checkPrepend)
        individual_tests(8) = it( &
                "prepending names to an empty list is still empty", &
                checkPrependToEmpty)
        tests = describe("MessageList_t", individual_tests)
    end function test_message_list

    pure function checkEmptyToString() result(result_)
        use Message_list_m, only: MessageList_t
        use Vegetables_m, only: Result_t, assertEmpty

        type(Result_t) :: result_

        type(MessageList_t) :: message_list

        result_ = assertEmpty(message_list%toString())
    end function checkEmptyToString

    pure function checkAppendToEmpty() result(result_)
        use Message_m, only: Info
        use Message_list_m, only: MessageList_t
        use Vegetables_m, only: Result_t, assertIncludes

        type(Result_t) :: result_

        character(len=*), parameter :: MESSAGE = "Test Message"
        type(MessageList_t) :: message_list

        message_list = message_list%appendMessage(Info( &
                "Some_module_m", "someProcedure", MESSAGE))

        result_ = assertIncludes(MESSAGE, message_list%toString())
    end function checkAppendToEmpty

    pure function checkAppendMultipleToEmpty() result(result_)
        use Message_m, only: Info, Warning
        use Message_list_m, only: MessageList_t
        use Vegetables_m, only: Result_t, assertIncludes

        type(Result_t) :: result_

        character(len=*), parameter :: FIRST_MESSAGE = "First Message"
        character(len=*), parameter :: SECOND_MESSAGE = "Second Message"
        type(MessageList_t) :: message_list1
        type(MessageList_t) :: message_list2

        message_list2 = message_list2%appendMessage(Info( &
                "Some_module_m", "someProcedure", FIRST_MESSAGE))
        message_list2 = message_list2%appendMessage(Warning( &
                "Some_module_m", "someProcedure", SECOND_MESSAGE))

        message_list1 = message_list1%appendMessages(message_list2)

        result_ = &
                assertIncludes(FIRST_MESSAGE, message_list1%toString()) &
                .and.assertIncludes(SECOND_MESSAGE, message_list1%toString())
    end function checkAppendMultipleToEmpty

    pure function checkAppendEmpty() result(result_)
        use Message_m, only: Info, Warning
        use Message_list_m, only: MessageList_t
        use Vegetables_m, only: Result_t, assertIncludes

        type(Result_t) :: result_

        character(len=*), parameter :: FIRST_MESSAGE = "First Message"
        character(len=*), parameter :: SECOND_MESSAGE = "Second Message"
        type(MessageList_t) :: message_list1
        type(MessageList_t) :: message_list2

        message_list1 = message_list1%appendMessage(Info( &
                "Some_module_m", "someProcedure", FIRST_MESSAGE))
        message_list1 = message_list1%appendMessage(Warning( &
                "Some_module_m", "someProcedure", SECOND_MESSAGE))

        message_list1 = message_list1%appendMessages(message_list2)

        result_ = &
                assertIncludes(FIRST_MESSAGE, message_list1%toString()) &
                .and.assertIncludes(SECOND_MESSAGE, message_list1%toString())
    end function checkAppendEmpty

    pure function checkCombineEmpty() result(result_)
        use Message_list_m, only: MessageList_t
        use Vegetables_m, only: Result_t, assertEmpty

        type(Result_t) :: result_

        type(MessageList_t) :: message_list1
        type(MessageList_t) :: message_list2

        message_list1 = message_list1%appendMessages(message_list2)

        result_ = assertEmpty(message_list1%toString())
    end function checkCombineEmpty

    pure function checkCombine() result(result_)
        use Message_m, only: Info, Warning
        use Message_list_m, only: MessageList_t
        use Vegetables_m, only: Result_t, assertIncludes

        type(Result_t) :: result_

        character(len=*), parameter :: FIRST_MESSAGE = "First Message"
        character(len=*), parameter :: SECOND_MESSAGE = "Second Message"
        character(len=*), parameter :: THIRD_MESSAGE = "Third Message"
        character(len=*), parameter :: FOURTH_MESSAGE = "Fourth Message"
        type(MessageList_t) :: message_list1
        type(MessageList_t) :: message_list2

        message_list1 = message_list1%appendMessage(Info( &
                "Some_module_m", "someProcedure", FIRST_MESSAGE))
        message_list1 = message_list1%appendMessage(Warning( &
                "Some_module_m", "someProcedure", SECOND_MESSAGE))

        message_list2 = message_list2%appendMessage(Info( &
                "Some_module_m", "someProcedure", THIRD_MESSAGE))
        message_list2 = message_list2%appendMessage(Warning( &
                "Some_module_m", "someProcedure", FOURTH_MESSAGE))

        message_list1 = message_list1%appendMessages(message_list2)

        result_ = &
                assertIncludes(FIRST_MESSAGE, message_list1%toString()) &
                .and.assertIncludes(SECOND_MESSAGE, message_list1%toString()) &
                .and.assertIncludes(THIRD_MESSAGE, message_list1%toString()) &
                .and.assertIncludes(FOURTH_MESSAGE, message_list1%toString())
    end function checkCombine

    pure function checkPrepend() result(result_)
        use Message_m, only: Info
        use Message_list_m, only: MessageList_t
        use Vegetables_m, only: Result_t, assertIncludes

        type(Result_t) :: result_

        character(len=*), parameter :: MODULE_NAME1 = "Some_module_m"
        character(len=*), parameter :: MODULE_NAME2 = "Another_module_m"
        character(len=*), parameter :: PROCEDURE_NAME1 = "someProcedure"
        character(len=*), parameter :: PROCEDURE_NAME2 = "anotherProcedure"
        type(MessageList_t) :: messages

        messages = messages%appendMessage(Info( &
                MODULE_NAME1, PROCEDURE_NAME1, "Test Message"))
        messages = messages%prependNames(MODULE_NAME2, PROCEDURE_NAME2)

        result_ = &
                assertIncludes(MODULE_NAME1, messages%toString()) &
                .and.assertIncludes(PROCEDURE_NAME1, messages%toString()) &
                .and.assertIncludes(MODULE_NAME2, messages%toString()) &
                .and.assertIncludes(PROCEDURE_NAME2, messages%toString())
    end function checkPrepend

    pure function checkPrependToEmpty() result(result_)
        use Message_list_m, only: MessageList_t
        use Vegetables_m, only: Result_t, assertEmpty

        type(Result_t) :: result_

        type(MessageList_t) :: messages

        messages = messages%prependNames("Another_module_m", "anotherProcedure")

        result_ = assertEmpty(messages%toString())
    end function checkPrependToEmpty
end module message_list_test

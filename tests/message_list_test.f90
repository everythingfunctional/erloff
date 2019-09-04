module message_list_test
    implicit none
    private

    public :: test_message_list
contains
    function test_message_list() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "converts to an empty string when it is empty", &
                checkEmptyToString)
        individual_tests(2) = it( &
                "can append to an empty list", checkAppendToEmpty)
        individual_tests(3) = it( &
                "can append multiple to an empty list", &
                checkAppendMultipleToEmpty)
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
        use strff, only: NEWLINE
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(MessageList_t) :: message_list
        character(len=*), parameter :: EXPECTED_STRING = &
                "Some_module_m.someProcedure:" // NEWLINE &
                // "    IN: Test Message"

        message_list = message_list%appendMessage(Info( &
                "Some_module_m", "someProcedure", "Test Message"))

        result_ = assertEquals(EXPECTED_STRING, message_list%toString())
    end function checkAppendToEmpty

    pure function checkAppendMultipleToEmpty() result(result_)
        use Message_m, only: Info, Warning
        use Message_list_m, only: MessageList_t
        use strff, only: NEWLINE
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(MessageList_t) :: message_list1
        type(MessageList_t) :: message_list2
        character(len=*), parameter :: EXPECTED_STRING = &
                "Some_module_m.someProcedure:" // NEWLINE &
                // "    IN: First Message" // NEWLINE &
                // "Some_module_m.someProcedure:" // NEWLINE &
                // "    WN: Second Message"

        message_list2 = message_list2%appendMessage(Info( &
                "Some_module_m", "someProcedure", "First Message"))
        message_list2 = message_list2%appendMessage(Warning( &
                "Some_module_m", "someProcedure", "Second Message"))

        message_list1 = message_list1%appendMessages(message_list2)

        result_ = assertEquals(EXPECTED_STRING, message_list1%toString())
    end function checkAppendMultipleToEmpty
end module message_list_test

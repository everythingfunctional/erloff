module message_list_test
    implicit none
    private

    public :: test_message_list
contains
    function test_message_list() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it( &
                "can create a new MessageList_t and convert it to a string", &
                checkToChar)
        tests = describe("MessageList_t", individual_tests)
    end function test_message_list

    pure function checkToChar() result(result_)
        use erloff, only: MessageList_t, Info, MessageList
        use strff, only: NEWLINE
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(MessageList_t) :: message_list
        character(len=*), parameter :: EXPECTED_STRING = &
                "Some_module_m.someProcedure:" // NEWLINE &
                // "    IN: Test Message"

        message_list = MessageList(Info( &
                "Some_module_m", "someProcedure", "Test Message"))

        result_ = assertEquals(EXPECTED_STRING, message_list%toString())
    end function checkToChar
end module message_list_test

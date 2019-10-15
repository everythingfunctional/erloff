module message_list_test
    implicit none
    private

    public :: test_message_list
contains
    function test_message_list() result(tests)
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = It( &
                "Converts to an empty string when it is empty", &
                checkEmptyToString)
        tests = Describe("MessageList_t", individual_tests)
    end function test_message_list

    function checkEmptyToString() result(result_)
        use Message_list_m, only: MessageList_t
        use Vegetables_m, only: Result_t, assertEmpty

        type(Result_t) :: result_

        type(MessageList_t) :: message_list

        result_ = assertEmpty(message_list%toString())
    end function checkEmptyToString
end module message_list_test

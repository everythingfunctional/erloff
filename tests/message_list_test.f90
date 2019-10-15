module message_list_test
    implicit none
    private

    public :: test_message_list
contains
    function test_message_list() result(tests)
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = It( &
                "Converts to an empty string when it is empty", &
                checkEmptyToString)
        individual_tests(2) = It( &
                "Can append a message to an empty list", &
                checkAppendToEmpty)
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
end module message_list_test

module message_test
    implicit none
    private

    public :: test_message
contains
    function test_message() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it( &
                "Can tell whether it is of a given type", checkType)
        tests = describe("Message_t", individual_tests)
    end function test_message

    pure function checkType() result(result_)
        use iso_varying_string, only: operator(//)
        use Message_m, only: Message_t, Info, INFO_TYPE
        use Vegetables_m, only: Result_t, assertNot, assertThat

        type(Result_t) :: result_

        class(Message_t), allocatable :: info_message

        allocate(info_message, source = Info( &
                "Some_module_m", "someProcedure", "Test Message"))
        result_ = &
                assertThat( &
                        info_message.isType.INFO_TYPE, &
                        info_message%repr() // ".isType." // INFO_TYPE%repr())
    end function checkType
end module message_test

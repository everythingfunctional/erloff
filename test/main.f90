program test
    implicit none

    call run()
contains
    subroutine run()
        use call_stack_entry_test, only: &
            call_stack_entry_call_stack_entry => test_call_stack_entry
        use call_stack_test, only: &
            call_stack_call_stack => test_call_stack
        use error_list_test, only: &
            error_list_error_list => test_error_list
        use message_list_test, only: &
            message_list_message_list => test_message_list
        use message_test, only: &
            message_message => test_message
        use iso_varying_string
        use Vegetables_m, only: TestItem_t, testThat, runTests

        type(TestItem_t) :: tests
        type(TestItem_t) :: individual_tests(5)

        individual_tests(1) = call_stack_entry_call_stack_entry()
        individual_tests(2) = call_stack_call_stack()
        individual_tests(3) = error_list_error_list()
        individual_tests(4) = message_list_message_list()
        individual_tests(5) = message_message()
        tests = testThat(individual_tests)

        call runTests(tests)
    end subroutine run
end program test

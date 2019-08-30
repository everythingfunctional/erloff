module placeholder_test
    implicit none
    private

    public :: test_placeholder
contains
    function test_placeholder() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it("Passes", placeholder)
        tests = describe("Placeholder", individual_tests)
    end function test_placeholder

    pure function placeholder() result(result_)
        use Vegetables_m, only: Result_t, succeed

        type(Result_t) :: result_

        result_ = succeed("Successfully")
    end function placeholder
end module placeholder_test

module Call_stack_m
    use Call_stack_entry_m, only: CallStackEntry_t

    implicit none
    private

    type, public :: CallStack_t
        private
        type(CallStackEntry_t), allocatable :: entries(:)
    contains
        private
        procedure, public :: prependNames
        procedure :: originatedFromModule
        procedure :: originatedFromProcedure
        generic, public :: operator(.originatedFrom.) => &
                originatedFromModule, originatedFromProcedure
        procedure, public :: repr
    end type CallStack_t

    public :: CallStack
contains
    function CallStack(module_, procedure_)
        use Call_stack_entry_m, only: CallStackEntry
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(CallStack_t) :: CallStack

        allocate(CallStack%entries(1))
        CallStack%entries(1) = CallStackEntry(module_, procedure_)
    end function CallStack

    subroutine prependNames(self, module_, procedure_)
        use Call_stack_entry_m, only: CallStackEntry_t, CallStackEntry
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        class(CallStack_t), intent(inout) :: self
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_

        type(CallStackEntry_t) :: previous_entries(size(self%entries))

        previous_entries = self%entries
        deallocate(self%entries)
        allocate(self%entries(size(previous_entries) + 1))
        self%entries(1) = CallStackEntry(module_, procedure_)
        self%entries(2:) = previous_entries
    end subroutine prependNames

    function originatedFromModule(self, module_)
        use Module_m, only: Module_t

        class(CallStack_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        logical :: originatedFromModule

        originatedFromModule = self%entries(size(self%entries)).isFrom.module_
    end function originatedFromModule

    function originatedFromProcedure(self, procedure_)
        use Procedure_m, only: Procedure_t

        class(CallStack_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        logical :: originatedFromProcedure

        originatedFromProcedure = self%entries(size(self%entries)).isFrom.procedure_
    end function originatedFromProcedure

    function repr(self)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, indent, join, NEWLINE

        class(CallStack_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        type(VARYING_STRING) :: entry_strings(size(self%entries))
        integer :: i

        do i = 1, size(self%entries)
            entry_strings = self%entries(i)%repr()
        end do

        repr = hangingIndent( &
                "CallStack_t(" // NEWLINE &
                    // "entries = [" // NEWLINE &
                    // indent( &
                            join(entry_strings, "," // NEWLINE), &
                            4) // NEWLINE // "]", &
                4) // NEWLINE // ")"
    end function repr
end module Call_stack_m

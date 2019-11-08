module Call_stack_m
    use Call_stack_entry_m, only: CallStackEntry_t, CallStackEntry
    use iso_varying_string, only: VARYING_STRING, operator(//)
    use Module_m, only: Module_t
    use Procedure_m, only: Procedure_t
    use strff, only: hangingIndent, indent, join, NEWLINE

    implicit none
    private

    type, public :: CallStack_t
        private
        type(CallStackEntry_t), allocatable :: entries(:)
    contains
        private
        procedure, public :: prependNames
        procedure, public :: toString
        procedure :: originatedFromModule
        procedure :: originatedFromProcedure
        generic, public :: operator(.originatedFrom.) => &
                originatedFromModule, originatedFromProcedure
        procedure :: includesModule
        procedure :: includesProcedure
        generic, public :: operator(.includes.) => &
                includesModule, includesProcedure
        procedure, public :: repr
    end type CallStack_t

    public :: CallStack
contains
    pure function CallStack(module_, procedure_)
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(CallStack_t) :: CallStack

        allocate(CallStack%entries(1))
        CallStack%entries(1) = CallStackEntry(module_, procedure_)
    end function CallStack

    pure subroutine prependNames(self, module_, procedure_)
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

    pure function toString(self) result(string)
        class(CallStack_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = join(self%entries%toString(), "->")
    end function toString

    pure function originatedFromModule(self, module_)
        class(CallStack_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        logical :: originatedFromModule

        originatedFromModule = self%entries(size(self%entries)).isFrom.module_
    end function originatedFromModule

    pure function originatedFromProcedure(self, procedure_)
        class(CallStack_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        logical :: originatedFromProcedure

        originatedFromProcedure = self%entries(size(self%entries)).isFrom.procedure_
    end function originatedFromProcedure

    pure function includesModule(self, module_)
        class(CallStack_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        logical :: includesModule

        includesModule = any(self%entries.isFrom.module_)
    end function includesModule

    pure function includesProcedure(self, procedure_)
        class(CallStack_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        logical :: includesProcedure

        includesProcedure = any(self%entries.isFrom.procedure_)
    end function includesProcedure

    pure function repr(self)
        class(CallStack_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        repr = hangingIndent( &
                "CallStack_t(" // NEWLINE &
                    // "entries = [" // NEWLINE &
                    // indent( &
                            join(self%entries%repr(), "," // NEWLINE), &
                            4) // NEWLINE // "]", &
                4) // NEWLINE // ")"
    end function repr
end module Call_stack_m

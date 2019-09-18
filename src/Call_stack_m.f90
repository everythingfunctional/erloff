module Call_stack_m
    use Call_stack_entry_m, only: CallStackEntry_t

    implicit none
    private

    type :: Node_t
        type(CallStackEntry_t), pointer :: entry_
        type(Node_t), pointer :: next
    end type Node_t

    type, public :: CallStack_t
        private
        integer :: depth
        type(Node_t), pointer :: head
        type(Node_t), pointer :: tail
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
        final :: destructor
    end type CallStack_t

    public :: CallStack
contains
    function CallStack(module_, procedure_)
        use Call_stack_entry_m, only: CallStackEntry
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(Module_t), pointer, intent(in) :: module_
        type(Procedure_t), pointer, intent(in) :: procedure_
        type(CallStack_t), pointer :: CallStack

        allocate(CallStack)
        allocate(CallStack%head)
        CallStack%head%entry_ => CallStackEntry(module_, procedure_)
        nullify(CallStack%head%next)
        CallStack%tail => CallStack%head
        CallStack%depth = 1
    end function CallStack

    subroutine prependNames(self, module_, procedure_)
        use Call_stack_entry_m, only: CallStackEntry
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        class(CallStack_t), intent(inout) :: self
        type(Module_t), pointer, intent(in) :: module_
        type(Procedure_t), pointer, intent(in) :: procedure_

        type(Node_t), pointer :: new_head

        allocate(new_head)
        new_head%entry_ => CallStackEntry(module_, procedure_)
        new_head%next => self%head
        self%head => new_head
        self%depth = self%depth + 1
    end subroutine prependNames

    function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: join

        class(CallStack_t), intent(in) :: self
        type(VARYING_STRING) :: string

        type(Node_t), pointer :: current
        type(VARYING_STRING) :: entry_strings(self%depth)
        integer :: i

        current => self%head
        do i = 1, self%depth
            entry_strings(i) = current%entry_%toString()
            current => current%next
        end do
        string = join(entry_strings, "->")
    end function toString

    pure function originatedFromModule(self, module_)
        use Module_m, only: Module_t

        class(CallStack_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        logical :: originatedFromModule

        originatedFromModule = self%tail%entry_.isFrom.module_
    end function originatedFromModule

    pure function originatedFromProcedure(self, procedure_)
        use Procedure_m, only: Procedure_t

        class(CallStack_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        logical :: originatedFromProcedure

        originatedFromProcedure = self%tail%entry_.isFrom.procedure_
    end function originatedFromProcedure

    function includesModule(self, module_)
        use Module_m, only: Module_t

        class(CallStack_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        logical :: includesModule

        type(Node_t), pointer :: current

        includesModule = .false.
        current => self%head
        do while (associated(current))
            if (current%entry_.isFrom.module_) then
                includesModule = .true.
                return
            end if
            current => current%next
        end do
    end function includesModule

    function includesProcedure(self, procedure_)
        use Procedure_m, only: Procedure_t

        class(CallStack_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        logical :: includesProcedure

        type(Node_t), pointer :: current

        includesProcedure = .false.
        current => self%head
        do while (associated(current))
            if (current%entry_.isFrom.procedure_) then
                includesProcedure = .true.
                return
            end if
            current => current%next
        end do
    end function includesProcedure

    function repr(self)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, join, NEWLINE

        class(CallStack_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        type(Node_t), pointer :: current
        type(VARYING_STRING) :: entry_strings(self%depth)
        integer :: i

        current => self%head
        do i = 1, self%depth
            entry_strings(i) = current%entry_%repr()
            current => current%next
        end do
        repr = hangingIndent( &
                "CallStack([" // NEWLINE &
                        // join(entry_strings, "," // NEWLINE) // "])", &
                4)
    end function repr

    subroutine destructor(self)
        type(CallStack_t), intent(inout) :: self

        type(Node_t), pointer :: current
        type(Node_t), pointer :: next

        current => self%head
        do while (associated(current))
            next => current%next
            deallocate(current%entry_)
            deallocate(current)
            current => next
        end do
    end subroutine destructor
end module Call_stack_m

module Call_stack_entry_m
    use Module_m, only: Module_t
    use Procedure_m, only: Procedure_t

    implicit none
    private

    type, public :: CallStackEntry_t
        private
        type(Module_t), pointer :: module
        type(Procedure_t), pointer :: procedure
    contains
        private
        procedure, public :: toString
        procedure :: isFromModule
        procedure :: isFromProcedure
        generic, public :: operator(.isFrom.) => &
                isFromModule, isFromProcedure
        procedure, public :: repr
        final :: desctructor
    end type CallStackEntry_t

    public :: CallStackEntry
contains
    function CallStackEntry(module_, procedure_) result(entry_)
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(Module_t), pointer, intent(in) :: module_
        type(Procedure_t), pointer, intent(in) :: procedure_
        type(CallStackEntry_t), pointer :: entry_

        allocate(entry_)
        entry_%module => module_
        entry_%procedure => procedure_
    end function CallStackEntry

    elemental function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)

        class(CallStackEntry_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%module%toString() // "." // self%procedure%toString()
    end function toString

    elemental function isFromModule(self, module_)
        use Module_m, only: Module_t

        class(CallStackEntry_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        logical :: isFromModule

        isFromModule = self%module == module_
    end function isFromModule

    elemental function isFromProcedure(self, procedure_)
        use Procedure_m, only: Procedure_t

        class(CallStackEntry_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        logical :: isFromProcedure

        isFromProcedure = self%procedure == procedure_
    end function isFromProcedure

    elemental function repr(self)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: NEWLINE

        class(CallStackEntry_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        repr = &
                'CallStackEntry(' // NEWLINE &
                // '    module = ' // self%module%repr() // ', ' // NEWLINE &
                // '    procedure = ' // self%procedure%repr() // ')'
    end function repr

    subroutine desctructor(self)
        type(CallStackEntry_t), intent(inout) :: self

        deallocate(self%module)
        deallocate(self%procedure)
    end subroutine desctructor
end module Call_stack_entry_m

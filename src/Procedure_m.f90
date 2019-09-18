module Procedure_m
    use iso_varying_string, only: VARYING_STRING

    implicit none
    private

    type, public :: Procedure_t
        private
        type(VARYING_STRING) :: name
    contains
        private
        procedure :: procedureEquals
        generic, public :: operator(==) => procedureEquals
        procedure, public :: toString
        procedure, public :: repr
    end type Procedure_t

    interface Procedure_
        module procedure ProcedureC
        module procedure ProcedureS
    end interface Procedure_

    public :: Procedure_
contains
    pure function ProcedureC(name)
        use iso_varying_string, only: assignment(=)

        character(len=*), intent(in) :: name
        type(Procedure_t), pointer :: ProcedureC

        allocate(ProcedureC)
        ProcedureC%name = name
    end function ProcedureC

    pure function ProcedureS(name)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: name
        type(Procedure_t), pointer :: ProcedureS

        allocate(ProcedureS)
        ProcedureS%name = name
    end function ProcedureS

    pure function procedureEquals(lhs, rhs)
        use iso_varying_string, only: operator(==)

        class(Procedure_t), intent(in) :: lhs
        type(Procedure_t), intent(in) :: rhs
        logical :: procedureEquals

        procedureEquals = lhs%name == rhs%name
    end function procedureEquals

    pure function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Procedure_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%name
    end function toString

    pure function repr(self)
        use iso_varying_string, only: VARYING_STRING, operator(//)

        class(Procedure_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        repr = 'Procedure_t("' // self%name // '")'
    end function repr
end module Procedure_m

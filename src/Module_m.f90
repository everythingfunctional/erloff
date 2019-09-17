module Module_m
    use iso_varying_string, only: VARYING_STRING

    implicit none
    private

    type, public :: Module_t
        private
        type(VARYING_STRING) :: name
    contains
        private
        procedure :: moduleEquals
        generic, public :: operator(==) => moduleEquals
    end type Module_t

    interface Module_
        module procedure ModuleC
        module procedure ModuleS
    end interface Module_

    public :: Module_
contains
    pure function ModuleC(name)
        use iso_varying_string, only: assignment(=)

        character(len=*), intent(in) :: name
        type(Module_t) :: ModuleC

        ModuleC%name = name
    end function ModuleC

    pure function ModuleS(name)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: name
        type(Module_t) :: ModuleS

        ModuleS%name = name
    end function ModuleS

    pure function moduleEquals(lhs, rhs)
        use iso_varying_string, only: operator(==)

        class(Module_t), intent(in) :: lhs
        type(Module_t), intent(in) :: rhs
        logical :: moduleEquals

        moduleEquals = lhs%name == rhs%name
    end function moduleEquals
end module Module_m

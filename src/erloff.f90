module erloff
    use Error_list_m, only: ErrorList_t, size
    use Message_m, only: &
            ! Types
            MessageType_t, &
            Message_t, &
            Debug_t, &
            Info_t, &
            Warning_t, &
            Error_t, &
            Fatal_t, &
            Internal_t, &
            ! Message constructors
            Debug, &
            Info, &
            Warning, &
            Fatal, &
            Internal, &
            ! Debug Levels
            GENERAL, &
            MEDIUM, &
            DETAILED, &
            NITTY_GRITTY, &
            ! Message types
            DEBUG_TYPE, &
            INFO_TYPE, &
            WARNING_TYPE, &
            ERROR_TYPE, &
            FATAL_TYPE, &
            INTERNAL_TYPE, &
            INPUTS_TYPE, &
            OUTPUTS_TYPE, &
            OUTSIDE_NORMAL_RANGE_TYPE, &
            UNEQUAL_ARRAY_SIZES_TYPE, &
            UNKNOWN_TYPE_TYPE
    use Message_list_m, only: MessageList_t, size
    use Module_m, only: Module_t, Module_
    use Procedure_m, only: Procedure_t, Procedure_

    implicit none
end module erloff

namespace Active.Net

module AccountManagement =

    open System.DirectoryServices.AccountManagement
    
    /// get the currently logged in user
    // can the user change while teh appl
    let currentUser = 
        // this returns machine-name/user: System.Security.Principal.WindowsIdentity.GetCurrent().Name
        // the following only the username. I'm not sure what is better...
        System.Environment.UserName

    /// get all groups where the given user is a member of
    let tryUserGroups(user: string) = 
        let context = new PrincipalContext(ContextType.Machine)
        let userPrincipal = UserPrincipal.FindByIdentity(context, IdentityType.SamAccountName, user);
        if(userPrincipal = null) then
            None
        else
            let groups = userPrincipal.GetGroups()
            groups |> Seq.cast<Principal> |> Some
    
    /// check if a user is member of the given group
    let userHasGroup(user: string, group: string) =
        let groups = tryUserGroups(user)
        groups.IsSome && groups.Value |> Seq.exists (fun principalGroup -> principalGroup.Name = group)

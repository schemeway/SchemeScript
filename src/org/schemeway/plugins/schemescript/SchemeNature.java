/*
 * Copyright (c) 2002-2003 Nu Echo Inc. All rights reserved.
 */
package org.schemeway.plugins.schemescript;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;

/**
 * @author Nu Echo Inc.
 */
public class SchemeNature implements IProjectNature
{
    private IProject mProject;
    
    public SchemeNature()
    {
        System.out.println("test");
    }
    
    public void configure() throws CoreException
    {
        System.out.println("Toto");
    }

    public void deconfigure() throws CoreException
    {
    }

    public IProject getProject()
    {
        return mProject;
    }

    public void setProject(IProject project)
    {
        mProject = project;
        
        try
        {
            IResource[] resources = project.members();
            System.out.println("number of resources = " + resources.length);
        }
        catch(CoreException exception)
        {
        }
    }
}

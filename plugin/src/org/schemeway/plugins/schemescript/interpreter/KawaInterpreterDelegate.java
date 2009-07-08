/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.interpreter;

import org.eclipse.core.runtime.*;
import org.eclipse.debug.core.*;
import org.eclipse.debug.core.model.*;

public class KawaInterpreterDelegate implements ILaunchConfigurationDelegate
{
    public KawaInterpreterDelegate()
    {
        super();
    }

    public void launch(ILaunchConfiguration configuration, String mode, ILaunch launch, IProgressMonitor monitor)
            throws CoreException
    {
        KawaProcess.getInstance().setLaunch(launch);
        launch.addProcess(KawaProcess.getInstance());
    }
}

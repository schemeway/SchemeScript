/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.interpreter;

import org.eclipse.core.runtime.*;
import org.eclipse.debug.core.*;
import org.eclipse.debug.core.model.*;

/**
 * @author SchemeWay Project.
 *
 */
public class SiscInterpreterDelegate implements ILaunchConfigurationDelegate {

    public SiscInterpreterDelegate()
    {
    }

    public void launch(ILaunchConfiguration configuration, String mode, ILaunch launch, IProgressMonitor monitor)
            throws CoreException
    {
        SiscProcess.getInstance().setLaunch(launch);
        launch.addProcess(SiscProcess.getInstance());
    }
}

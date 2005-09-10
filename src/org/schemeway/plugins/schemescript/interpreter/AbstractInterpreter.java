/*
 * Copyright (c) 2004-2005 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.interpreter;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;

public abstract class AbstractInterpreter implements Interpreter
{

    public abstract IInterpreterProcess getProcess();
    
    public abstract String getConfigurationType();
    
    public boolean isRunning() {
        return getProcess().isRunning();
    }

    public void start() {
        IInterpreterProcess instance = getProcess(); 
        if (instance != null && instance.getLaunch() == null) {
            try {
                ILaunchConfigurationType configType = DebugPlugin.getDefault()
                                                                 .getLaunchManager()
                                                                 .getLaunchConfigurationType(getConfigurationType());
                ILaunchConfigurationWorkingCopy copy = configType.newInstance(null, "");
                copy.launch(ILaunchManager.RUN_MODE, new NullProgressMonitor());
            }
            catch (CoreException exception) {
            }
        }
        if (instance != null && instance.getLaunch() != null)
            showConsole();
    }

    public void stop() {
        IInterpreterProcess process = getProcess();
        if (process.canTerminate()) {
            try {
                process.terminate();
            } 
            catch (DebugException e) {
            }
        }
    }

    public void restart() {
        if (isRunning()) {
            stop();
        }
        start();
    }

    public boolean supportInterruption() {
        return false;
    }

    public void interrupt() {
    }

    public void eval(String code) {
        if (!isRunning())
            start();
        getProcess().sendToInterpreter(code + "\n");
    }

    public void load(IFile file) {
        String filename = file.getRawLocation().toString();
        eval("(load \"" + filename + "\")");
    }

    public void showConsole() {
        IProcess process = getProcess();
        if (process == null || process.getLaunch() == null)
            return;
        
        IConsole console = DebugUITools.getConsole(process);
        ConsolePlugin.getDefault().getConsoleManager().showConsoleView(console);
        IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
        IViewPart view = page.findView("org.eclipse.ui.console.ConsoleView");
        if (view != null)
            view.setFocus();
    }
}

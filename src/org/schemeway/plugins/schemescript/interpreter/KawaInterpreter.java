/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.interpreter;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.debug.core.*;
import org.eclipse.debug.ui.*;
import org.eclipse.ui.*;
import org.eclipse.ui.console.*;
import org.schemeway.plugins.schemescript.*;

public class KawaInterpreter implements Interpreter {
    public static final String CONFIG_TYPE = SchemeScriptPlugin.PLUGIN_NS + ".kawaInterpreter";

    public KawaInterpreter() {
        super();
    }

    public void showConsole() {
        IConsole console = DebugUITools.getConsole(KawaProcess.getInstance());
        ConsolePlugin.getDefault().getConsoleManager().showConsoleView(console);
        IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
        IViewPart view = page.findView("org.eclipse.ui.console.ConsoleView");
        if (view != null)
            view.setFocus();
    }

    public boolean isRunning() {
        return true;
    }

    public void start() {
        if (KawaProcess.getInstance().getLaunch() == null) {
            try {
                ILaunchConfigurationType configType = DebugPlugin.getDefault()
                                                                 .getLaunchManager()
                                                                 .getLaunchConfigurationType(CONFIG_TYPE);
                ILaunchConfigurationWorkingCopy copy = configType.newInstance(null, "");
                copy.launch(ILaunchManager.RUN_MODE, new NullProgressMonitor());
            }
            catch (CoreException exception) {
            }
        }
        showConsole();
    }

    public void stop() {
    }

    public void restart() {
        start();
    }

    public boolean supportInterruption() {
        return false;
    }

    public void interrupt() {
    }

    public void eval(String code) {
        KawaProcess.sendToInterpreter(code);
    }

    public void load(IFile file) {
        String filename = file.getRawLocation().toString();
        eval("(load \"" + filename + "\")");
    }
}
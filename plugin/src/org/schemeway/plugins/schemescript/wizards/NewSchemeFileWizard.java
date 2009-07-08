/*
 * Copyright (c) 2002-2003 Nu Echo Inc. All rights reserved.
 */
package org.schemeway.plugins.schemescript.wizards;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.jface.wizard.*;
import org.eclipse.ui.*;
import org.eclipse.ui.part.*;
import org.schemeway.plugins.schemescript.*;

/**
 * @author Nu Echo Inc.
 */
public class NewSchemeFileWizard extends Wizard implements INewWizard
{
    IWorkbench mWorkbench;
    IStructuredSelection mSelection;
    NewSchemeFileWizardPage mMainPage;

    public void init(IWorkbench workbench, IStructuredSelection selection)
    {
        this.mWorkbench = workbench;
        this.mSelection = selection;
        setWindowTitle("Scheme File");
    }

    public void addPages()
    {
        mMainPage = new NewSchemeFileWizardPage("Kawa Scheme File", mSelection);
        addPage(mMainPage);
    }

//    public boolean canFinish()
//    {
//        return super.canFinish() && mSelection.size() == 1 && ((mSelection.getFirstElement() instanceof IFolder)
//                || (mSelection.getFirstElement() instanceof IProject));
//    }

    public boolean performFinish()
    {
        try
        {
            String filename = mMainPage.getFileName();
            if (!filename.endsWith(".scm"))
            {
                mMainPage.setFileName(filename + ".scm");
            }
            IFile file = mMainPage.createNewFile();
            FileEditorInput input = new FileEditorInput(file);
            mWorkbench.getActiveWorkbenchWindow().getActivePage().openEditor(input, SchemeScriptPlugin.PLUGIN_NS + ".editor.SchemeEditor");
        }
        catch(CoreException exception)
        {
        }
        return true;
    }
    
//    public IFile getFile(String name) throws CoreException
//    {
//        Object selectedItem = mSelection.getFirstElement();
//        if (selectedItem instanceof IProject)
//            return ((IProject)selectedItem).getFile(name);
//        else 
//            return ((IFolder) selectedItem).getFile(name);
//    }

}

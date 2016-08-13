{-|
Module      : Jupyter.Install
Description : Utilities for installing Jupyter kernels.
Copyright   : (c) Andrew Gibiansky, 2016
License     : MIT
Maintainer  : andrew.gibiansky@gmail.com
Stability   : stable
Portability : POSIX

Before any Jupyter frontends (such as the notebook or console) can use a kernel, the kernel must be installed.
Traditionally, kernels are installed by defining a kernelspec and running @jupyter kernelspec install@, where a kernelspec
is defined by creating a directory with a set of predefined files that Jupyter knows how to handle, and is installed by
passing the directory to @jupyter kernelspec install@. Installed kernelspecs may be queried with @jupyter kernelspec list@.

Instead, this module provides a few utilities for defining, installing, and locating kernelspecs. A kernelspec
can be defined by creating a value of type 'Kernelspec' and can be installed with 'installKernel'. The
installed kernelspecs may be listed or searched with 'findKernels' and 'findKernel', respectively. These utilities
are simply convenient wrappers around the @jupyter kernelspec install@ and @jupyter kernelspec list@ commands.
-}
module Jupyter.Install (
  -- * Kernelspec Definitions
  Kernelspec(..),
  simpleKernelspec,

  -- * Installing Jupyter Kernels
  installKernel,
  InstallUser(..),
  InstallResult(..),

  -- * Detecting Installed Kkernels
  findKernel,
  findKernels,
  ) where

-- Imports from 'text'
import Data.Text (Text)

import Jupyter.Install.Internal 

-- | Utility for creating simple kernelspecs, with all optional 'Kernelspec' fields initialized to their empty values.
--
-- Example for Python 3:
-- >>> simpleKernelspec "Python 3" "python3" $ \exe0 connFile = ["python", "-m", "ipykernel", "-f", connFile]
simpleKernelspec :: Text -- ^ The kernel display name (see 'kernelspecDisplayName').
                 -> Text -- ^ The kernel language name (see 'kernelspecLanguage').
                 -> (FilePath -> FilePath -> [String]) -- ^ The kernel command line invocation (see 'kernelspecCommand').
                 -> Kernelspec
simpleKernelspec displayName language command =
    Kernelspec {
        kernelspecDisplayName = displayName,
        kernelspecLanguage = language,
        kernelspecCommand = command,
        kernelspecJsFile = Nothing,
        kernelspecLogoFile = Nothing,
        kernelspecEnv = mempty
      }

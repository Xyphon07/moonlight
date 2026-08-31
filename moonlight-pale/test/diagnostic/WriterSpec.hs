module WriterSpec
  ( tests,
  )
where

import Moonlight.Pale.Diagnostic.Core
  ( Diagnosed,
    DiagnosticSeverity (..),
    emitDiagnostic,
    filterBySeverity,
    pureDiagnosed,
    runDiagnosed,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

data WriterNote = WriterNote
  { writerNoteSeverity :: DiagnosticSeverity,
    writerNoteMessage :: String
  }
  deriving stock (Eq, Show)

tests :: TestTree
tests =
  testGroup
    "pale.diagnostic.writer"
    [ testCase "runDiagnosed returns the value and emitted notes in emission order" $
        assertEqual
          "diagnosed writer result"
          ("accepted", [infoNote, warningNote, errorNote])
          (runDiagnosed workedDiagnosed),
      testCase "filterBySeverity keeps notes at or above the threshold" $
        assertEqual
          "severity-filtered notes"
          [warningNote, errorNote]
          (filterBySeverity writerNoteSeverity DiagWarning writerNotes)
    ]

workedDiagnosed :: Diagnosed WriterNote String
workedDiagnosed =
  emitDiagnostic infoNote
    *> emitDiagnostic warningNote
    *> emitDiagnostic errorNote
    *> pureDiagnosed "accepted"

writerNotes :: [WriterNote]
writerNotes =
  [ infoNote,
    warningNote,
    errorNote
  ]

infoNote :: WriterNote
infoNote =
  WriterNote
    { writerNoteSeverity = DiagInfo,
      writerNoteMessage = "local section observed"
    }

warningNote :: WriterNote
warningNote =
  WriterNote
    { writerNoteSeverity = DiagWarning,
      writerNoteMessage = "overlap pending"
    }

errorNote :: WriterNote
errorNote =
  WriterNote
    { writerNoteSeverity = DiagError,
      writerNoteMessage = "gluing obstruction"
    }

------------
title: The Real Real World of Haskell
subtitle: Relations on the Land of Pure Functional Programming by a Mortal Being
author: Arnaud Bailly
date: 2016-04-22
theme: old-english-devoxx
------------


## Table of Contents

* How I Came to Reside in the Land of Haskell and Why It Matters
* On Various Aspects of Haskelland Civilisation and The Mores of Its Inhabitants
* Some Ways this Wonderful Journey Could Inspire the Reader

# How I Came to Visit Haskelland

----

![](/images/geographie.jpg)

## Where the author dares to present himself

![](/images/ebeniste.jpg)

## On the Company which I travelled to Haskelland for

![](/images/cm-website.png)

## Where it is demonstrated why the interest of the honorable reader might be aroused

* This is not a Haskell tutorial
* This is not a Monad tutorial
* Let's stop thinking *"Haskell is good for your brain but not for practical use"*
* I will be happy if you end up thinking: *I could try this cool stuff in my next microservice*
* I will be even happier if you can put to **actual use** some of the stuff I present here

# On Various Aspects of Haskelland

-----

![](/images/frontispice.jpg)

----

Where the author tries to cover the most important aspects of Haskeller's civilisation in an extremely short span of time, thus
probably missing lot of important points and generating frustration in the informed reader, but nevertheless hoping to convey enough
tips for the audience to be willing to adopt some of the customs of those strange people.

# Language

## On some minor differences between Haskell Language and more Common ones

![](/images/analyse.jpg)

## On the importance of composition in Haskell

![](/images/musique.jpg)

-----

```haskell
transactionsFromCashflows :: AccountId
                          -> (CashFlow -> [Transaction])
                          -> [CashFlow] -> [Transaction]
transactionsFromCashflows pivot generator =
    concatMap $ map (normalize . balance pivot) . generator
```

## On the virtue of being lazy

![](/images/femme-allongee.jpg)

## On some of the less desirable aspects of the language

![](/images/catacombes.jpg)

# Philosophy

## On the importance of Types for Haskellers

![](/images/astronomie.jpg)

## Where one discovers types do not impose extra burden at runtime


```haskell
newtype AccountNr = AccountNr { accountNr :: T.Text }
                    deriving (Eq,Ord,Show,Read)


instance IsString AccountNr where
    fromString = AccountNr . T.pack
```

## Where it is shown types can even conjure the undead

```haskell
data Base64
data Hex
newtype Encoded code = Encoded { encodedText :: Text }
toBase64Text :: ByteString -> Encoded Base64
toHex        :: ByteString -> Encoded Hex
```

## Where one discovers subtle differences cause more trouble than obvious ones

```haskell
class (Eq a) => Differ a where
  uniqueId    :: a -> Id a
  constructor :: a -> Entity b
  equals      :: a -> a -> Bool
```
## Where one starts to wonder if there is an end to recursion

```haskell
class (ToJSON (Command a)) => BusinessModel a where
  data Event a   :: *
  data Command a :: *
```

```haskell
type family Id a :: *
type instance Id Account       = AccountId
type instance Id Transaction   = TransactionId
```

## Where it is shown how more constraints makes one more Free

```haskell
data Gratis f a = Effectless a
      | forall x. Effectful (f x) (x -> Gratis f a)

data EmailServiceF a where
  DoMail :: Emailer -> Email -> EmailServiceF EmailStatus
  GetAllEmails :: Confirmation -> EmailServiceF [EmailWithStatus]

type MailService = Gratis EmailServiceF

```

```haskell
liftFF :: EmailServiceF a -> MailService a
liftFF c = Effectful c pure

doMail :: Emailer -> Email -> MailService EmailStatus
doMail mailer mail = liftFF $ DoMail mailer mail

doGetAllEmails :: Confirmation -> MailService [ EmailWithStatus ]
doGetAllEmails = liftFF . GetAllEmails
```

```haskell
interpret :: MailService a -> ExceptT L.Text (WebStateM s l m) a
interpret (Effectful (DoMail mailer mail) f)        =
  lift (liftIO $ mailer mail >>= handleSendingResult) >>= interpret . f
interpret (Effectful (GetAllEmails confirmation) f) =
  lift (runWithEmails $ doGetEmails confirmation)     >>= interpret . f
interpret (Effectless a)                            = return a
```

# Mores & Daily Life

## Where the amazed foreigner discovers how much can be done with crude tools

![](/images/menuisier.jpg)

------

![](/images/emacs.png)

------

![](/images/ihaskell.png)


## On the usefulness and efficiency of interacting with the machine

![](/images/tourneur.jpg)

## On the typical day of a commoner in Haskelland

1. Write a skeletal test file, e.g. `test/FooTest.hs`
1. Start REPL in Emacs by loading file `C-c C-l`
1. See it fail to compile
1. `:r`eload until it compiles
1. Run the test and see it fail: `hspec myTest`
1. Fill in code until test passes
1. Do a full compile and test run before pushing to CI

# Government

## Where we discover Haskellers also value experimentation

![](/images/chymie.jpg)

## Where we see empiricism going hand in hand with formalism

```haskell
instance Arbitrary ScaleRatio where  arbitrary = ...
instance Arbitrary Transaction where  arbitrary = ...
```

```haskell
prop_scaled_transaction_is_normalized :: Transaction -> ScaleRatio
                                       -> Bool
prop_scaled_transaction_is_normalized tx (ScaleRatio ratio) =
  isNormalized tx' && isBalanced tx'
  where
    tx' = scale ratio tx
```

## Where one discovers QuickCheck can be useful beside testing properties

### Generating sample data to test migration

```haskell
sample_v8_Events :: [ByteString]
sample_v8_Events =
  ["{\"tag\":\"AddedDocuments\",\"contents\":[\"c1f09023b3a9398a1d8a257c372392ab\",[]]}"
  , "{\"tag\":\"UpdatedInvestor\",\"contents\":{\"invDocuments\":[],
     \"invBankAccount\":{\"bankBranch\":\"Ge\",\"bankAccountName\":\"1W\",\"bankAccountNo\":\"nN\",\"bankName\":\"AU\"},
     \"invId\":\"46ed3336bc60a5a423962e9b6343c003\",\"invReferralCode\":{\"refer\":\"\"},\"invAccepttandcs\":false,
     \"invLegalEntity\":{\"tag\":\"Corporate\",\"primaryContactPosition\":{\"tag\":\"OtherPosition\",\"contents\":\"jZ\"},\"primaryContact\":{\"personTitle\":\"Y8\",\"personFirstName\":\"qf\",\"personLastName\":\"pz\",\"personIdNumber\":\"yC\",\"personTelephone\":\"nC\",\"personEmail\":\"xwmRJAstJRrvBy1x8FnJ4Y2.6nmGy@opBpRq.sg\"},\"company\":{\"companyPostcode\":\"kT\",\"companyName\":\"-o\",\"companyUEN\":\"LA\",\"legalForm\":\"LTD\",\"companyAddress\":\"oh\"}}}}"
   , "{\"tag\":\"NoEvent\",\"contents\":[]}"
   , "{\"tag\":\"AddedDocuments\",\"contents\":[\"b9973afdd569f11269fe0be736086049\",[]]}"
```

----

### Generating execution sequences representing typical scenarios

```haskell
genAdminActions =
  frequency [ (1,  return [])
            , (1,  do
                   f <- NewListing <$> arbitrary <*> newFacility <*> rate <*> risk
                   (f:) <$> genAdminActions)
            , (10, (AcceptSomeFacility:) <$> genAdminActions)
            , (5,  (LookupFacilities:) <$> genAdminActions)
            , (6,  (AdvanceDay:) . (ConfirmRepayments:) <$> genAdminActions)
            ]
```

## On the use of different level of testing

![](/images/pyramide.jpg)

# Architecture

## On the importance of laying out strong foundations

![](/images/architecture.jpg)

## On the intricacies of building complex software with a Cabal of scholars

![](/images/horlogerie.jpg)

## Where Haskellers demonstrate how easily they can adopt foreigners practice

![](/images/event-sourcing.png)

----

![](/images/services-architecture.png)

# Diplomacy

## Where we separate that which is pure from that which is impure

![](/images/pure-impure.jpg)

## Where we learn we can have our cake and eat it too

![](/images/boulanger.jpg)

## Where Haskellers prove they know how to communicate with the REST of the world ##

![](/images/marine.jpg)

## Where some types make mortal beings doubt about their sanity

```haskell
type CreateJob = ReqBody '[JSON] Job :> Post '[JSON] JobId
type ListJobs  = Get '[JSON] [Job]
type RemoveJob = Capture "jobid" JobId :> Delete '[JSON] JobId

type SchedulerApi = "api" :> "scheduler" :> "jobs" :> CreateJob
               :<|> "api" :> "scheduler" :> "jobs" :> ListJobs
               :<|> "api" :> "scheduler" :> "jobs" :> RemoveJob
```

## On some ways data is persisted to stable storage

![](/images/marbrerie.jpg)

# War

## Where peaceful Haskellers prepare themselves to wage war

![](/images/machines-de-guerre.jpg)

----

```haskell
lendingHost :: Service
            -> Property HasInfo
lendingHost svc = propertyList $ props
    & installLatestDocker
    & dockerComposeInstalled
    & File.dirExists certPath
    & writeSslKey "nginx-private-key" "lending.capital-match.com"
    & restrictToOwner (certPath </> "ssl.key")
    & writeCertificateChain "nginx-public-cert" "lending.capital-match.com"
    & (certPath <> "ssl-unified.crt") `File.mode`
      combineModes [ownerWriteMode, ownerReadMode]
    & hasDataContainer "cm-data" dataImage
    & composeUp True "/home/build/docker-compose.yml"
      (Just [("ROOT_PASSWORD",rootPassword)])
```

## On the war-time organisation of Haskell's legions

![](/images/militaire.jpg)

----

```haskell
    ImageName "capitalmatch/nginx" `buildImageBy` \imageName -> do
       need ["nginx/Dockerfile","nginx/reload_nginx.sh"
            , "nginx/forego", "nginx/Procfile"]
       cmdDockerBuild imageName "nginx"

    "docker//source-version.txt" %> \versionFile -> do
      alwaysRerun
      Stdout v <- cmd "git" ["rev-parse", "HEAD"]
      writeFileChanged versionFile v
```

## On the capabilities to wage war outside of Haskelland

![](/images/marine-formation.jpg)

----

![](/images/system-architecture.png)

# Conclusion

## What benefits one can expect from visiting Haskelland

![](/images/paix-utrecht.jpg)

## What the cautious traveller should be aware of when ashoring Haskelland

![](/images/rhinoceros.jpg)

# Acknowledgements

## Credits

All the woodcuts illustrating this talk are drawn from the [Encyclopédie de Diderot et d'Alembert](http://planches.eu) with the exception of:

* [Durer's "Artist drawing a couching woman"](http://40.media.tumblr.com/8b72c0e766b096a0d83c6a98f6586a1d/tumblr_n8m8bas4Xe1tgg8dko1_1280.jpg),
* The [Austerlitz Pyramid](https://upload.wikimedia.org/wikipedia/commons/3/3b/Pyramide_Austerlitz_1805.jpg),
* [Durer's Rhinoceros](https://alicegonella.files.wordpress.com/2013/04/durers-rhino-1515.jpg),
* [Allégorie à la Paix d'Utrecht](http://gallica.bnf.fr/ark:/12148/btv1b8408177f/f1.highres).

## Thanks

* To all the fine people at Capital Match who made all this possible,
* To Haskellers all over the world who fuel the fire with their wits and spirit,
* To Haskell B. Curry who, among others, laid out the foundations for our daily job.

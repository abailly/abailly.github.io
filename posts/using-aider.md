---
title: Vibe coding with Aider
subtitle: Experimenting with the latest development hype
author: Arnaud Bailly
date: 2025-03-28
---

Nudged by [this post](https://bsky.app/profile/simonwillison.net/post/3lgsxsw6ma22z) on Blue Sky from Simon Willison, I decided yesterady to give [Aider](https://aider.chat) a try while coding some stuff on [amaru](https://github.com/pragma-org/amaru), and I must say I have been quite impressed by the fluidity of the interaction.

Setting it up was pretty much flawless once I stopped using `pip` directly and just used `homebrew`. I tried to configure it to use my local ollama but the first results were very disappointing, so I tried to use one of the models proposed by [Mistral](https://mistral.ai). After creating an account, I wasn't able to convince Aider to use my account keys so I just gave up after a few minutes and switched to [Anthropic](https://anthropic.com/). I had to buy $5 of credits to get an API key (no free plan there) and then got started.

The first task I asked it was:

```
> write a function into crates/amaru-kernel/src/network.rs to load a EraHistory structure from a JSON file
```

It produced the following function (I omit the body for reasons that will be clear later):

```rust
/// Load an `EraHistory` from a JSON file.
///
/// # Arguments
///
/// * `path` - Path to the JSON file containing era history data
///
/// # Returns
///
/// Returns a Result containing the `EraHistory` if successful, or a `String` if the file
/// cannot be read or parsed.
///
/// # Example
///
/// ```no_run
/// use amaru_kernel::network::load_era_history_from_file;
/// use std::path::Path;
///
/// let era_history = load_era_history_from_file(Path::new("era_history.json")).unwrap();
/// ```
pub fn load_era_history_from_file(path: &Path) -> Result<EraHistory, String> {
...
}
```

and I only had to fix some missing package to have it compiled. I then asked

```
> add a unit test converting `PREPROD_ERA_HISTORY` to a JSON file and reading it back again, checking the two matches
```

It added the required test, yielding the following test. I only had to remove the superfluous comments which were just repeating what the code was doing and were not particularly informative.

```
    #[test]
    fn test_era_history_json_serialization() {
        let original_era_history = &*PREPROD_ERA_HISTORY;

        let mut temp_file_path = env::temp_dir();
        temp_file_path.push("test_era_history.json");

        let json_data = serde_json::to_string_pretty(original_era_history)
            .expect("Failed to serialize EraHistory to JSON");

        let mut file = File::create(&temp_file_path).expect("Failed to create temporary file");

        file.write_all(json_data.as_bytes())
            .expect("Failed to write JSON data to file");

        let loaded_era_history = load_era_history_from_file(temp_file_path.as_path())
            .expect("Failed to load EraHistory from file");

        assert_eq!(
            *original_era_history, loaded_era_history,
            "Era histories don't match"
        );

        std::fs::remove_file(temp_file_path).ok();
    }
```

I had to do some manual tweaks to have the test pass, notably use a temporary file in the `tempdir` to add the JSON file to converted, but it was pretty straightforward. Then I asked for a few more tweaks:

```
> instead of returning a String as error from load_era_history_from_file, create a proper error type named EraHistoryFileError, distinguishing the different error cases, and return it from the function
```

It modified the function, giving me:

```rust
pub fn load_era_history_from_file(path: &Path) -> Result<EraHistory, EraHistoryFileError> {
    let file = File::open(path).map_err(EraHistoryFileError::FileOpenError)?;
    let reader = BufReader::new(file);

    serde_json::from_reader(reader).map_err(EraHistoryFileError::JsonParseError)
}
```

along with the accompanying definition for `EraHistoryFileError` containing the above variants.

My next prompt was to ask for

```
> add one test for each failure case
```

which it fulfilled with the following tests:

```rust
    #[test]
    fn test_era_history_file_open_error() {
        let non_existent_path = Path::new("non_existent_file.json");
        ...
    }

    #[test]
    fn test_era_history_json_parse_error() {
        let mut temp_file_path = env::temp_dir();
        temp_file_path.push("invalid_era_history.json");

        let invalid_json = r#"{ "eras": [invalid json] }"#;
        ...
    }
```

The complete result is visible in [this file](https://github.com/pragma-org/amaru/blob/main/crates/amaru-kernel/src/network.rs).

Another task I gave to aider was to

```
> implement a method for EraHistory that compute the relative slot in the epoch given an absolute slot
```

again, it quickly generated the following function, complete with the accompanying error handling which preexisted in the module, along with an test case.

```rust
    /// Computes the relative slot in the epoch given an absolute slot.
    ///
    /// Returns the number of slots since the start of the epoch containing the given slot.
    ///
    /// # Errors
    ///
    /// Returns `TimeHorizonError::PastTimeHorizon` if the slot is beyond the time horizon.
    /// Returns `TimeHorizonError::InvalidEraHistory` if the era history is invalid.
    pub fn slot_in_epoch(&self, slot: u64) -> Result<u64, TimeHorizonError> {
        let epoch = self.slot_to_epoch(slot)?;
        let bounds = self.epoch_bounds(epoch)?;
        Ok(slot - bounds.start)
    }
```

I only had to request to:

```
> add a test for slot_in_epoch for error cases
```

to generate the following unit test:

```rust
    #[test]
    fn slot_in_epoch_past_time_horizon() {
        let eras = one_era();
        let relative_slot = eras.slot_in_epoch(864001);
        assert_eq!(relative_slot, Err(TimeHorizonError::PastTimeHorizon));
    }

```

Once convinced of the capabilities of Aider, I thought it would be nice to not have to switch away from Emacs and it turned that, obviously, there was an Emacs mode available aptly named [aider.el](https://github.com/tninja/aider.el), which provides the necessary bindings to interact with Aider inside Emacs.

I am obviously jumping quite late on the AI bandwagon, and not totally embracing it, but I found this small experiment compelling enough I know want to use on a regular basis.

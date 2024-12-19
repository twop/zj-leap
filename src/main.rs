use std::collections::BTreeMap;
use zellij_tile::prelude::*;

// #[derive(Clone)]
// struct Theme {
//     tab_name: PaletteColor,
//     pane_name: PaletteColor,
//     jump_key: PaletteColor,
//     disabled: PaletteColor,
// }

struct Initializing {
    tabs: Option<Vec<TabInfo>>,
    mode: Option<ModeInfo>,
    panes: Option<PaneManifest>,
}

impl Initializing {
    fn try_transition(
        &self,
        tabs: Option<Vec<TabInfo>>,
        mode: Option<ModeInfo>,
        panes: Option<PaneManifest>,
    ) -> State {
        match (
            tabs.as_ref().or(self.tabs.as_ref()),
            mode.as_ref().or(self.mode.as_ref()),
            panes.as_ref().or(self.panes.as_ref()),
        ) {
            (Some(tabs), Some(_mode), Some(panes)) => State::Active(Active::new(tabs, panes)),
            (tabs, mode, panes) => State::Initializing(Initializing {
                tabs: tabs.cloned(),
                mode: mode.cloned(),
                panes: panes.cloned(),
            }),
        }
    }
}

enum State {
    Initializing(Initializing),
    Active(Active),
}

enum Ev {
    Key(BareKey),
    RecievedTabs(Vec<TabInfo>),
    RecievedMod(ModeInfo),
    RecievedPanes(PaneManifest),
}

enum Action {
    OpenTabIndex(u32),
    FocusOnPane(PaneId),
    Close,
}

fn t() {
    // serialize_text_with_coordinates(text, x, y, width, height)
}
#[derive(Clone)]
enum Sequence {
    One(char),
    Two(char, char),
}

impl From<&Sequence> for String {
    fn from(sequence: &Sequence) -> Self {
        match sequence {
            Sequence::One(c) => c.to_string(),
            Sequence::Two(c1, c2) => format!("{}{}", c1, c2),
        }
    }
}

#[derive(Clone)]
struct SequenceGenerator {
    single: String,
    double: String,
    index: usize,
}

#[derive(Clone)]
struct Pane {
    name: String,
    id: PaneId,
    sequence: Sequence,
    is_focused: bool,
    is_floating: bool,
}

#[derive(Clone)]
struct Tab {
    current: bool,
    are_floating_panes_visible: bool,
    name: String,
    sequence: Sequence,
    selectable_panes: Vec<Pane>,
    panes_count: usize,
}

#[derive(Clone)]
struct Active {
    tabs: Vec<Tab>,
    combo: String,
    // theme: Theme,
    key_generator: SequenceGenerator,
    hide_panes: bool,

    // cache
    sourse_tabs: Vec<TabInfo>,
    sourse_panes: PaneManifest,
}

impl Default for State {
    fn default() -> Self {
        Self::Initializing(Initializing {
            tabs: None,
            mode: None,
            panes: None,
        })
    }
}

impl SequenceGenerator {
    fn new(single_chars: &str, double_chars: &str) -> Self {
        Self {
            single: single_chars.to_lowercase(),
            double: double_chars.to_lowercase(),
            index: 0,
        }
    }

    fn reset(&self) -> Self {
        Self {
            single: self.single.clone(),
            double: self.double.clone(),
            index: 0,
        }
    }

    fn next(&mut self) -> Sequence {
        let index = self.index;
        self.index += 1;

        match index {
            i if i < self.single.len() => Sequence::One(self.single.chars().nth(i).unwrap()),
            i => {
                let effective_index = i - self.single.len();
                let base = effective_index / (self.double.len() + self.single.len());
                let second = effective_index % (self.double.len() + self.single.len());

                Sequence::Two(
                    self.double.chars().nth(base).unwrap(),
                    self.single
                        .chars()
                        .chain(self.double.chars())
                        .nth(second)
                        .unwrap(),
                )
            }
        }
    }
}

// yeah, I know that Im creating a new state every time, and it is wasteful and all
// can't help myself to get some joy from writing it like that where it doesn't matter even a bit
fn update_state(state: &State, ev: Ev) -> (Option<State>, Option<Action>) {
    match (state, ev) {
        (State::Initializing(initializing), Ev::RecievedTabs(tabs)) => (
            Some(initializing.try_transition(Some(tabs), None, None)),
            None,
        ),

        (State::Initializing(initializing), Ev::RecievedMod(mods)) => (
            Some(initializing.try_transition(None, Some(mods), None)),
            None,
        ),

        (State::Initializing(initializing), Ev::RecievedPanes(panes)) => (
            Some(initializing.try_transition(None, None, Some(panes))),
            None,
        ),

        (_, Ev::Key(BareKey::Esc | BareKey::Enter)) => (None, Some(Action::Close)),

        (State::Active(active), Ev::RecievedTabs(tabs)) => {
            let mut generator = active.key_generator.reset();
            (
                Some(State::Active(Active {
                    tabs: create_tabs(
                        &tabs,
                        &active.sourse_panes,
                        active.hide_panes,
                        &mut generator,
                    ),
                    combo: "".to_string(),
                    key_generator: generator,
                    sourse_tabs: tabs,
                    sourse_panes: active.sourse_panes.clone(),
                    hide_panes: active.hide_panes,
                })),
                None,
            )
        }

        (State::Active(active), Ev::RecievedPanes(panes)) => {
            let mut generator = active.key_generator.reset();
            (
                Some(State::Active(Active {
                    tabs: create_tabs(
                        &active.sourse_tabs,
                        &panes,
                        active.hide_panes,
                        &mut generator,
                    ),
                    combo: "".to_string(),
                    key_generator: generator,
                    sourse_tabs: active.sourse_tabs.clone(),
                    sourse_panes: panes,
                    hide_panes: active.hide_panes,
                })),
                None,
            )
        }

        (State::Active(active), Ev::Key(key)) => match key {
            BareKey::Char(ch)
                if active.key_generator.single.contains(ch)
                    || active.key_generator.double.contains(ch) =>
            {
                let new_combo = format!("{}{}", active.combo, ch);
                match check_overall_tree(active.tabs.iter(), &new_combo) {
                    TreeMatch::MatchTab(index) => (None, Some(Action::OpenTabIndex(index as u32))),
                    TreeMatch::MatchPane(id) => (None, Some(Action::FocusOnPane(id))),
                    TreeMatch::Miss => (None, Some(Action::Close)),
                    TreeMatch::Possible => (
                        Some(State::Active(Active {
                            combo: new_combo,
                            ..active.clone()
                        })),
                        None,
                    ),
                }
            }

            BareKey::Char(' ') => {
                let mut generator = active.key_generator.reset();
                let hide_panes = !active.hide_panes;
                (
                    Some(State::Active(Active {
                        tabs: create_tabs(
                            &active.sourse_tabs,
                            &active.sourse_panes,
                            hide_panes,
                            &mut generator,
                        ),
                        combo: "".to_string(),
                        hide_panes,
                        key_generator: generator,
                        ..active.clone()
                    })),
                    None,
                )
            }

            _ => (None, None),
        },

        _ => (None, None),
    }
}

const SINGLE_CHARS_PALETTE: &str = "fjdkghmvcru";
const DOUBLE_CHARS_PALETTE: &str = "slaw";

impl Active {
    fn new(tabs: &[TabInfo], panes: &PaneManifest) -> Self {
        // take from settings
        let mut generator = SequenceGenerator::new(SINGLE_CHARS_PALETTE, DOUBLE_CHARS_PALETTE);

        let hide_panes = false;
        Active {
            tabs: create_tabs(tabs, panes, hide_panes, &mut generator),
            combo: "".to_string(),
            // theme: Theme::from_mode_palette(&mode_info.style.colors),
            key_generator: generator,
            sourse_tabs: tabs.iter().cloned().collect(),
            sourse_panes: panes.clone(),
            hide_panes,
        }
    }
}

// impl Theme {
//     fn from_mode_palette(palette: &Palette) -> Self {
//         Self {
//             tab_name: palette.blue,
//             pane_name: palette.blue,
//             jump_key: palette.gold,
//             disabled: palette.gray,
//         }
//     }
// }

fn create_tabs(
    tabs: &[TabInfo],
    panes: &PaneManifest,
    hide_panes: bool,
    key_gen: &mut SequenceGenerator,
) -> Vec<Tab> {
    let include_pane =
        |p: &PaneInfo| -> bool { p.is_selectable && !(p.is_floating && p.is_plugin) };

    tabs.iter()
        .enumerate()
        .map(|(i, info)| Tab {
            current: info.active,
            are_floating_panes_visible: info.are_floating_panes_visible,
            name: info.name.clone(),
            sequence: key_gen.next(),
            panes_count: panes
                .panes
                .get(&i)
                .map(|panes| panes.iter().filter(|p| include_pane(p)).count())
                .unwrap_or(0),
            selectable_panes: match hide_panes {
                true => vec![],
                false => panes
                    .panes
                    .get(&i)
                    .map(|panes| {
                        panes
                            .iter()
                            .filter_map(|p| {
                                include_pane(p).then(|| Pane {
                                    name: p.title.clone(),
                                    id: match p.is_plugin {
                                        true => PaneId::Plugin(p.id),
                                        false => PaneId::Terminal(p.id),
                                    },
                                    is_floating: p.is_floating,
                                    sequence: key_gen.next(),
                                    is_focused: p.is_focused,
                                })
                            })
                            .collect()
                    })
                    .unwrap_or_default(),
            },
        })
        .collect()
}

enum SequenceMatch {
    FullMatch,
    Miss,
    Possible,
}

fn check_combo(sequence: &Sequence, combo: &str) -> SequenceMatch {
    match (sequence, combo.len()) {
        (_, 0) => SequenceMatch::Possible,

        (Sequence::One(k), 1) => match combo.chars().next() {
            Some(c) if c == *k => SequenceMatch::FullMatch,
            _ => SequenceMatch::Miss,
        },
        (Sequence::Two(k1, k2), 2) => {
            let mut chars = combo.chars();
            match chars.next().zip(chars.next()) {
                Some((ch1, ch2)) if ch1 == *k1 && ch2 == *k2 => SequenceMatch::FullMatch,
                _ => SequenceMatch::Miss,
            }
        }

        (Sequence::Two(k1, _), 1) if combo.chars().next() == Some(*k1) => SequenceMatch::Possible,

        _ => SequenceMatch::Miss,
    }
}

enum TreeMatch {
    MatchTab(usize),
    MatchPane(PaneId),
    Miss,
    Possible,
}
fn check_overall_tree<'a>(tabs: impl Iterator<Item = &'a Tab>, combo: &str) -> TreeMatch {
    use SequenceMatch as S;
    use TreeMatch as T;
    tabs.enumerate()
        .flat_map(|(i, tab)| {
            let tab_match = match check_combo(&tab.sequence, combo) {
                S::FullMatch => T::MatchTab(i),
                S::Miss => T::Miss,
                S::Possible => T::Possible,
            };

            tab.selectable_panes
                .iter()
                .map(|pane| match check_combo(&pane.sequence, combo) {
                    S::FullMatch => T::MatchPane(pane.id),
                    S::Miss => T::Miss,
                    S::Possible => T::Possible,
                })
                .chain(Some(tab_match))
        })
        .fold(T::Miss, |check_res, cur_match| {
            match (check_res, cur_match) {
                (T::MatchTab(index), _) | (_, T::MatchTab(index)) => T::MatchTab(index),
                (T::MatchPane(id), _) | (_, T::MatchPane(id)) => T::MatchPane(id),
                (T::Miss, T::Possible) | (T::Possible, T::Possible) | (T::Possible, T::Miss) => {
                    T::Possible
                }
                (T::Miss, T::Miss) => T::Miss,
            }
        })
}

enum ColorRole {
    /// magenta
    HotKey = 3,
    /// beige
    Primary = 0,
    /// cyan
    Accent = 1,
    /// white
    Default = 4,
    /// green
    Highlight = 2,
}

struct TextPart {
    text: String,
    color_role: ColorRole,
}

struct TextBuilder {
    parts: Vec<TextPart>,
}

impl TextBuilder {
    fn start() -> Self {
        Self { parts: vec![] }
    }

    fn text(mut self, text: impl Into<String>) -> Self {
        self.parts.push(TextPart {
            text: text.into(),
            color_role: ColorRole::Default,
        });
        self
    }

    fn colored(mut self, text: impl Into<String>, role: ColorRole) -> Self {
        self.parts.push(TextPart {
            text: text.into(),
            color_role: role,
        });
        self
    }

    fn space(self) -> Self {
        self.text(" ")
    }

    fn len(&self) -> usize {
        self.parts.iter().map(|part| part.text.len()).sum()
    }

    fn to_upper(mut self) -> Self {
        for part in &mut self.parts {
            part.text = part.text.to_uppercase();
        }
        self
    }

    fn build(self) -> Text {
        let mut text = String::new();
        for part in &self.parts {
            text.push_str(&part.text);
        }

        self.parts
            .into_iter()
            .fold((Text::new(text), 0), |(text_so_far, pos), part| {
                let end = pos + part.text.chars().count();
                (
                    text_so_far.color_range(part.color_role as usize, pos..end),
                    end,
                )
            })
            .0
    }
}

// -----
register_plugin!(State);

impl ZellijPlugin for State {
    fn load(&mut self, _configuration: BTreeMap<String, String>) {
        request_permission(&[
            PermissionType::ReadApplicationState,
            PermissionType::ChangeApplicationState,
        ]);

        subscribe(&[
            EventType::TabUpdate,
            EventType::PaneUpdate,
            EventType::ModeUpdate,
            EventType::Key,
        ]);
    }

    fn update(&mut self, event: Event) -> bool {
        let Some(ev) = (match event {
            Event::TabUpdate(tab_info) => Some(Ev::RecievedTabs(tab_info)),
            Event::ModeUpdate(info) => Some(Ev::RecievedMod(info)),
            Event::PaneUpdate(manifest) => Some(Ev::RecievedPanes(manifest)),
            Event::Key(key) if key.has_no_modifiers() => Some(Ev::Key(key.bare_key)),
            _ => None,
        }) else {
            return false;
        };

        match update_state(self, ev) {
            (_, Some(action)) => {
                match action {
                    Action::OpenTabIndex(index) => {
                        switch_tab_to(index + 1); // 1 based
                        close_self()
                    }
                    Action::Close => close_self(),
                    Action::FocusOnPane(id) => {
                        focus_pane_with_id(id, true);
                        close_self()
                    }
                };

                false
            }
            (Some(new_state), None) => {
                *self = new_state;
                true
            }
            (None, None) => false,
        }
    }

    #[allow(unused_mut)]
    fn render(&mut self, rows: usize, cols: usize) {
        use ColorRole as R;
        match self {
            State::Initializing(_) => (),
            State::Active(active) => {
                fn format_progress(seq: &Sequence, combo: &str) -> TextBuilder {
                    let builder = TextBuilder::start().colored("", ColorRole::Highlight);

                    let builder = match (check_combo(seq, combo), seq) {
                        (SequenceMatch::FullMatch, Sequence::One(_)) => builder.space().text(seq),
                        (SequenceMatch::FullMatch, Sequence::Two(_, _)) => builder.text(seq),
                        (SequenceMatch::Miss, Sequence::One(_)) => {
                            builder.space().colored(seq, R::Accent)
                        }
                        (SequenceMatch::Miss, Sequence::Two(_, _)) => {
                            builder.colored(seq, R::Accent)
                        }
                        (SequenceMatch::Possible, Sequence::One(_)) => {
                            builder.space().colored(seq, R::HotKey)
                        }
                        (SequenceMatch::Possible, Sequence::Two(ch1, ch2)) => match combo.len() {
                            0 => builder.colored(seq, R::HotKey),
                            _ => builder
                                .colored(ch1.to_string(), R::Accent)
                                .colored(ch2.to_string(), R::HotKey),
                        },
                    };

                    builder.to_upper()
                }
                // let plugin = active
                //     .sourse_panes
                //     .panes
                //     .values()
                //     .flat_map(|panes| panes.iter())
                //     .find(|p| {
                //         p.is_plugin
                //             && p.plugin_url
                //                 .as_ref()
                //                 .map(|url| url.contains(".wasm"))
                //                 .unwrap_or(false)
                //     });
                // println!()

                // println!("Plugin: {:#?}", plugin);

                let mut row_pos = 0;
                for tab in active.tabs.iter() {
                    let start_row = row_pos;

                    let tab_text = format_progress(&tab.sequence, &active.combo)
                        .colored(" - ", R::Accent)
                        .colored(
                            &tab.name,
                            match tab.current {
                                true => R::Primary,
                                false => R::Accent,
                            },
                        );

                    let tab_text = match (active.hide_panes, tab.selectable_panes.len()) {
                        (true, _) => tab_text
                            .text(" (")
                            .colored(tab.panes_count.to_string(), R::Highlight)
                            .text(" panes)"),
                        (false, 1) => {
                            let trimmed = trim_mid(&tab.selectable_panes[0].name, 24, 6);
                            tab_text.colored(" | ", R::Highlight).text(trimmed)
                        }
                        _ => tab_text,
                    };

                    print_text_with_coordinates(tab_text.build(), 1, row_pos, None, None);

                    row_pos += 1;

                    if tab.selectable_panes.len() >= 2 {
                        for pane in tab.selectable_panes.iter() {
                            let focused = pane.is_focused;
                            let pane_text = format_progress(&pane.sequence, &active.combo)
                                .colored(
                                    match (
                                        focused,
                                        pane.is_floating,
                                        tab.are_floating_panes_visible,
                                    ) {
                                        (true, true, true) | (true, false, false) => " * ",
                                        (true, false, true)
                                        | (true, true, false)
                                        | (false, _, _) => " - ",
                                    },
                                    R::Accent,
                                )
                                .colored(
                                    if pane.is_floating { "\u{f0554} " } else { "" },
                                    R::Accent,
                                )
                                .colored(
                                    match pane.id {
                                        PaneId::Terminal(_) => "",

                                        // wasm symbol from nerd fonts
                                        PaneId::Plugin(_) => "\u{e6a1} ",
                                    },
                                    R::Accent,
                                )
                                .colored(trim_mid(&pane.name, 30, 7), R::Default);

                            print_text_with_coordinates(pane_text.build(), 3, row_pos, None, None);
                            row_pos += 1;
                        }
                    }

                    if tab.current {
                        // ╭ - light top left rounded corner
                        // │ - light vertical bar
                        // ╰ - light bottom left rounded corner
                        // ┃ - heavy mid

                        let heavy_mid = TextBuilder::start().colored('┃', R::Primary).build();

                        for row in start_row..row_pos {
                            print_text_with_coordinates(heavy_mid.clone(), 0, row, None, None);
                        }
                    }

                    row_pos += 1;
                }

                let legend = TextBuilder::start()
                    .text("Help: ")
                    .text("type ")
                    .colored("letters", R::HotKey)
                    .text(" to jump. ")
                    .colored("<Esc>", R::HotKey)
                    .text(" - Close,")
                    .colored(" <Space>", R::HotKey)
                    .text(" - Toggle panes");
                let legend_len = legend.len();

                print_text_with_coordinates(
                    legend
                        .text(" ".repeat(cols - (legend_len).min(cols)))
                        .build()
                        .opaque(),
                    0,
                    rows - 1,
                    Some(cols),
                    None,
                );
            }
        }
    }
}

fn trim_mid(s: &str, limit: usize, take_first: usize) -> String {
    match s.len() {
        n if n <= limit => s.to_string(),
        _ if limit < take_first || limit - take_first <= 2 => format!("{}..", &s[0..limit - 2]),
        _ => format!(
            "{}..{}",
            &s[0..take_first],
            &s[s.len() + take_first + 2 - limit..]
        ),
    }
}

#[cfg(test)]
mod test {
    use std::arch::asm;

    use super::*;

    #[test]
    fn test_check_combo() {
        use Sequence::*;
        assert!(matches!(
            check_combo(&One('a'), "a"),
            SequenceMatch::FullMatch
        ));
        assert!(matches!(check_combo(&One('a'), "b"), SequenceMatch::Miss));
        assert!(matches!(check_combo(&One('a'), "ab"), SequenceMatch::Miss));
        assert!(matches!(
            check_combo(&One('a'), ""),
            SequenceMatch::Possible
        ));

        assert!(matches!(
            check_combo(&Two('a', 'b'), "ab"),
            SequenceMatch::FullMatch
        ));

        assert!(matches!(
            check_combo(&Two('a', 'b'), "a"),
            SequenceMatch::Possible
        ));
        assert!(matches!(
            check_combo(&Two('a', 'b'), "ac"),
            SequenceMatch::Miss
        ));
        assert!(matches!(
            check_combo(&Two('a', 'b'), "b"),
            SequenceMatch::Miss
        ));
        assert!(matches!(
            check_combo(&Two('a', 'b'), ""),
            SequenceMatch::Possible
        ));
    }

    #[test]
    fn test_check_combo_on_tabs() {
        use Sequence::*;
        let tabs = vec![
            Tab {
                name: "Tab 1".to_string(),
                current: false,
                are_floating_panes_visible: true,
                sequence: One('a'),
                selectable_panes: vec![
                    Pane {
                        name: "Pane 1".to_string(),
                        id: PaneId::Terminal(1),
                        sequence: Two('b', 'c'),
                        is_focused: true,
                        is_floating: true,
                    },
                    Pane {
                        name: "Pane 2".to_string(),
                        id: PaneId::Terminal(2),
                        sequence: One('d'),
                        is_floating: false,
                        is_focused: false,
                    },
                ],
                panes_count: 2,
            },
            Tab {
                current: true,
                are_floating_panes_visible: false,
                name: "Tab 2".to_string(),
                sequence: Two('e', 'f'),
                selectable_panes: vec![
                    Pane {
                        is_floating: false,
                        name: "Pane 3".to_string(),
                        id: PaneId::Terminal(3),
                        sequence: One('g'),
                        is_focused: false,
                    },
                    Pane {
                        is_floating: false,
                        name: "Pane 4".to_string(),
                        id: PaneId::Terminal(4),
                        sequence: Two('h', 'i'),
                        is_focused: false,
                    },
                ],
                panes_count: 4,
            },
        ];

        assert!(matches!(
            check_overall_tree(tabs.iter(), "a"),
            TreeMatch::MatchTab(0)
        ));

        assert!(matches!(
            check_overall_tree(tabs.iter(), "bc"),
            TreeMatch::MatchPane(PaneId::Terminal(1))
        ));

        assert!(matches!(
            check_overall_tree(tabs.iter(), "b"),
            TreeMatch::Possible
        ));

        assert!(matches!(
            check_overall_tree(tabs.iter(), "x"),
            TreeMatch::Miss
        ));

        assert!(matches!(
            check_overall_tree(tabs.iter(), "ef"),
            TreeMatch::MatchTab(1)
        ));

        assert!(matches!(
            check_overall_tree(tabs.iter(), "hi"),
            TreeMatch::MatchPane(PaneId::Terminal(4))
        ));

        assert!(matches!(
            check_overall_tree(tabs.iter(), ""),
            TreeMatch::Possible
        ));
    }
    #[test]
    fn test_trim_mid() {
        assert_eq!(trim_mid("hello", 10, 7), "hello");
        assert_eq!(trim_mid("hello world", 7, 7), "hello..");
        assert_eq!(trim_mid("hello world", 8, 4), "hell..ld");
        assert_eq!(trim_mid("very long string here", 10, 4), "very..here");
        assert_eq!(trim_mid("short", 5, 3), "short");
        assert_eq!(trim_mid("another test string", 12, 7), "another..ing");
        assert_eq!(trim_mid("x", 5, 2), "x");
        assert_eq!(trim_mid("abcdefghijk", 5, 2), "ab..k");
    }
}

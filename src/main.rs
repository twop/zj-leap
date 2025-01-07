use itertools::Itertools;
use prelude::Direction;
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
        let plugin_ids = get_plugin_ids();
        match (
            tabs.as_ref().or(self.tabs.as_ref()),
            mode.as_ref().or(self.mode.as_ref()),
            panes.as_ref().or(self.panes.as_ref()),
        ) {
            (Some(tabs), Some(_mode), Some(panes)) => {
                State::Active(Active::new(tabs, panes, plugin_ids))
            }
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
    JumpTo(Address),
    Close,
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
    address: Address,
    sequence: Option<Sequence>,
    is_focused: bool,
    is_floating: bool,
}

#[derive(Clone)]
struct Tab {
    current: bool,
    address: Address,
    are_floating_panes_visible: bool,
    is_fullscreen_active: bool,
    name: String,
    sequence: Sequence,
    panes: Vec<Pane>,
}

#[derive(Clone, PartialEq, Eq, Copy)]
enum VisualMode {
    ShowOnlyTabs,
    ShowTabsAndPanes,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Address {
    Tab(usize),
    Pane(usize, PaneId),
}

#[derive(Clone)]
struct Active {
    tabs: Vec<Tab>,
    combo: String,
    // theme: Theme,
    key_symbols: KeySymbols,
    key_generator: SequenceGenerator,
    mode: VisualMode,
    cursor: Address,

    // cache
    plugin_ids: PluginIds,
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

        (State::Active(active), Ev::RecievedTabs(tabs)) => {
            let mut generator = active.key_generator.reset();
            (
                Some(State::Active(Active {
                    tabs: create_tabs(
                        &tabs,
                        &active.sourse_panes,
                        active.mode,
                        &active.plugin_ids,
                        &mut generator,
                    ),
                    combo: "".to_string(),
                    key_generator: generator,
                    sourse_tabs: tabs,
                    ..active.clone() //
                                     // sourse_panes: active.sourse_panes.clone(),

                                     // mode: active.mode,
                                     // plugin_ids: active.plugin_ids.clone(),
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
                        active.mode,
                        &active.plugin_ids,
                        &mut generator,
                    ),
                    combo: "".to_string(),
                    key_generator: generator,
                    sourse_panes: panes,
                    ..active.clone()
                })),
                None,
            )
        }

        (_, Ev::Key(BareKey::Esc)) => (None, Some(Action::Close)),

        (State::Active(active), Ev::Key(key)) => match key {
            BareKey::Char(ch)
                if active.key_generator.single.contains(ch)
                    || active.key_generator.double.contains(ch) =>
            {
                let new_combo = format!("{}{}", active.combo, ch);
                match check_overall_tree(active.tabs.iter(), &new_combo) {
                    TreeMatch::Match(address) => (None, Some(Action::JumpTo(address))),
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
                let mode = match active.mode {
                    VisualMode::ShowOnlyTabs => VisualMode::ShowTabsAndPanes,
                    VisualMode::ShowTabsAndPanes => VisualMode::ShowOnlyTabs,
                };
                (
                    Some(State::Active(Active {
                        tabs: create_tabs(
                            &active.sourse_tabs,
                            &active.sourse_panes,
                            mode,
                            &active.plugin_ids,
                            &mut generator,
                        ),
                        combo: "".to_string(),
                        mode,
                        cursor: match (mode, active.cursor) {
                            (VisualMode::ShowOnlyTabs, Address::Pane(tab_index, _)) => {
                                Address::Tab(tab_index)
                            }
                            _ => active.cursor,
                        },
                        key_generator: generator,
                        ..active.clone()
                    })),
                    None,
                )
            }

            BareKey::Up => (
                Some(State::Active(Active {
                    cursor: active
                        .cursor
                        .move_cursor(active.mode, &active.tabs, Direction::Up),
                    ..active.clone()
                })),
                None,
            ),

            BareKey::Down => (
                Some(State::Active(Active {
                    cursor: active
                        .cursor
                        .move_cursor(active.mode, &active.tabs, Direction::Down),
                    ..active.clone()
                })),
                None,
            ),

            BareKey::Enter => (None, Some(Action::JumpTo(active.cursor))),

            _ => (None, None),
        },

        _ => (None, None),
    }
}

const SINGLE_CHARS_PALETTE: &str = "fjdkghmvcru";
const DOUBLE_CHARS_PALETTE: &str = "slaw";

impl Active {
    fn new(tabs: &[TabInfo], panes: &PaneManifest, plugin_ids: PluginIds) -> Self {
        // take from settings
        let mut generator = SequenceGenerator::new(SINGLE_CHARS_PALETTE, DOUBLE_CHARS_PALETTE);

        let mode = VisualMode::ShowOnlyTabs;
        let visual_tabs = create_tabs(tabs, panes, mode, &plugin_ids, &mut generator);
        let cursor = Address::init(&visual_tabs);
        Active {
            tabs: visual_tabs,
            combo: "".to_string(),
            // theme: Theme::from_mode_palette(&mode_info.style.colors),
            key_generator: generator,
            sourse_tabs: tabs.iter().cloned().collect(),
            sourse_panes: panes.clone(),
            key_symbols: Default::default(),
            plugin_ids,
            mode,
            cursor,
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
    mode: VisualMode,
    plugin_ids: &PluginIds,
    key_gen: &mut SequenceGenerator,
) -> Vec<Tab> {
    let include_pane =
        // |p: &PaneInfo| -> bool { p.is_selectable && !(p.is_floating && p.is_plugin) };
        |p: &PaneInfo| -> bool {p.id != plugin_ids.plugin_id && p.is_selectable};
    tabs.iter()
        .enumerate()
        .map(|(tab_index, info)| Tab {
            current: info.active,
            address: Address::Tab(tab_index),
            are_floating_panes_visible: info.are_floating_panes_visible,
            is_fullscreen_active: info.is_fullscreen_active,
            name: info.name.clone(),
            sequence: key_gen.next(),
            panes: panes
                .panes
                .get(&tab_index)
                .map(|panes| {
                    panes
                        .iter()
                        .filter_map(|p| {
                            include_pane(p).then(|| {
                                let id = match p.is_plugin {
                                    true => PaneId::Plugin(p.id),
                                    false => PaneId::Terminal(p.id),
                                };

                                Pane {
                                    name: p.title.clone(),
                                    address: Address::Pane(tab_index, id),
                                    id,
                                    is_floating: p.is_floating,
                                    sequence: match mode {
                                        // for only tabs just not actually generate the keys
                                        // it is possible that tabs should use shorter keys, and then panes
                                        // will use longer ones
                                        VisualMode::ShowOnlyTabs => None,
                                        VisualMode::ShowTabsAndPanes => Some(key_gen.next()),
                                    },
                                    is_focused: p.is_focused,
                                }
                            })
                        })
                        .collect()
                })
                .unwrap_or_default(),
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
    Match(Address),
    Miss,
    Possible,
}

fn check_overall_tree<'a>(tabs: impl Iterator<Item = &'a Tab>, combo: &str) -> TreeMatch {
    use SequenceMatch as S;
    use TreeMatch as T;
    tabs.enumerate()
        .flat_map(|(i, tab)| {
            let tab_match = match check_combo(&tab.sequence, combo) {
                S::FullMatch => T::Match(tab.address),
                S::Miss => T::Miss,
                S::Possible => T::Possible,
            };

            tab.panes
                .iter()
                .filter_map(|pane| {
                    pane.sequence
                        .as_ref()
                        .map(|sequence| match check_combo(&sequence, combo) {
                            S::FullMatch => T::Match(pane.address),
                            S::Miss => T::Miss,
                            S::Possible => T::Possible,
                        })
                })
                .chain(Some(tab_match))
        })
        .fold(T::Miss, |check_res, cur_match| {
            match (check_res, cur_match) {
                (T::Match(address), _) | (_, T::Match(address)) => T::Match(address),
                (T::Miss, T::Possible) | (T::Possible, T::Possible) | (T::Possible, T::Miss) => {
                    T::Possible
                }
                (T::Miss, T::Miss) => T::Miss,
            }
        })
}

// really not a huge fan of zellij theming api
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
    is_opaque: bool,
}

impl TextBuilder {
    fn start() -> Self {
        Self {
            parts: vec![],
            is_opaque: false,
        }
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

    fn opaque(mut self) -> Self {
        self.is_opaque = true;
        self
    }

    fn add(mut self, other: TextBuilder) -> Self {
        self.parts.extend(other.parts);
        self
    }

    fn build(self) -> Text {
        let mut text = String::new();
        for part in &self.parts {
            text.push_str(&part.text);
        }

        let res = self
            .parts
            .into_iter()
            .fold((Text::new(text), 0), |(text_so_far, pos), part| {
                let end = pos + part.text.chars().count();
                (
                    text_so_far.color_range(part.color_role as usize, pos..end),
                    end,
                )
            })
            .0;

        match self.is_opaque {
            true => res.opaque(),
            false => res,
        }
    }
}

impl Address {
    fn init(tabs: &[Tab]) -> Address {
        tabs.iter()
            .enumerate()
            .find_map(|(index, tab)| tab.current.then(|| Address::Tab(index)))
            .unwrap_or(Address::Tab(0))
    }

    fn move_cursor(&self, mode: VisualMode, tabs: &[Tab], direction: Direction) -> Address {
        let delta = match direction {
            Direction::Left | Direction::Right => return *self,
            Direction::Up => -1,
            Direction::Down => 1,
        };

        match mode {
            VisualMode::ShowOnlyTabs => {
                let cursor_tab = match self {
                    Address::Tab(index) | Address::Pane(index, _) => *index,
                };

                Address::Tab(
                    (((cursor_tab + tabs.len()) as i32 + delta) % tabs.len() as i32) as usize,
                )
            }

            VisualMode::ShowTabsAndPanes => {
                let next_cursor = tabs
                    .iter()
                    .enumerate()
                    .flat_map(|(tab_index, tab)| {
                        std::iter::once((Address::Tab(tab_index), tab.panes.len())).chain(
                            tab.panes.iter().map(move |pane| {
                                (Address::Pane(tab_index, pane.id), tab.panes.len())
                            }),
                        )
                    })
                    // this one removes solo pane items that in tabs
                    .filter_map(|(item, panes_count)| match item {
                        Address::Pane(_, _) if panes_count <= 1 => None,
                        to_keep => Some(to_keep),
                    })
                    .tuple_windows()
                    .chain(tabs.last().map(|last_tab| {
                        // this one wraps to the begining again, which captures
                        // the case when we are on the first item with up direction

                        let last_tab_index = tabs.len() - 1;
                        (
                            match last_tab.panes.len() {
                                0 | 1 => Address::Tab(last_tab_index),

                                panes_cnt => {
                                    Address::Pane(last_tab_index, last_tab.panes[panes_cnt - 1].id)
                                }
                            },
                            Address::Tab(0),
                        )
                    }))
                    .find_map(|(prev, next)| match delta {
                        1 => (&prev == self).then(|| next),
                        -1 => (&next == self).then(|| prev),
                        _ => Some(*self),
                    });

                next_cursor.unwrap_or(Address::Tab(0))
            }
        }
    }
}

// taken from gitui codebase here: https://github.com/extrawurst/gitui/blob/27e28d5f5141be43648b93dc05a164a08dd4ef96/src/keys/symbols.rs
// please don't judge me for creatively appropriating it, P.S. thanks Stephan (https://github.com/extrawurst)
#[derive(Debug, Clone)]
pub struct KeySymbols {
    pub enter: String,
    pub left: String,
    pub right: String,
    pub up: String,
    pub down: String,
    pub backspace: String,
    pub home: String,
    pub end: String,
    pub page_up: String,
    pub page_down: String,
    pub tab: String,
    pub back_tab: String,
    pub delete: String,
    pub insert: String,
    pub esc: String,
    pub control: String,
    pub shift: String,
    pub alt: String,
}

#[rustfmt::skip]
impl Default for KeySymbols {
	fn default() -> Self {
		Self {
			enter: "\u{23ce}".into(),     //⏎
			left: "\u{2190}".into(),      //←
			right: "\u{2192}".into(),     //→
			up: "\u{2191}".into(),        //↑
			down: "\u{2193}".into(),      //↓
			backspace: "\u{232b}".into(), //⌫
			home: "\u{2912}".into(),      //⤒
			end: "\u{2913}".into(),       //⤓
			page_up: "\u{21de}".into(),   //⇞
			page_down: "\u{21df}".into(), //⇟
			tab: "\u{21e5}".into(),       //⇥
			back_tab: "\u{21e4}".into(),  //⇤
			delete: "\u{2326}".into(),    //⌦
			insert: "\u{2380}".into(),    //⎀
			esc: "\u{238b}".into(),       //⎋
			control: "^".into(),
			shift: "\u{21e7}".into(),     //⇧
			alt: "\u{2325}".into(),       //⌥
		}
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
                    Action::Close => close_self(),
                    Action::JumpTo(id) => {
                        match id {
                            Address::Tab(tab_index) => {
                                switch_tab_to(tab_index as u32 + 1); // 1 based
                                close_self();
                            }
                            Address::Pane(tab_index, pane_id) => {
                                switch_tab_to(tab_index as u32 + 1); // 1 based
                                focus_pane_with_id(pane_id, true);
                                close_self();
                            }
                        }
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
        if rows == 0 || cols == 0 {
            return;
        }

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
                    // let start_row = row_pos;

                    let tab_text = TextBuilder::start()
                        .add(format_progress(&tab.sequence, &active.combo))
                        .colored(
                            match tab.current {
                                true => " * ",
                                false => " - ",
                            },
                            R::Accent,
                        )
                        .colored(
                            match tab.is_fullscreen_active {
                                true => "\u{f50c} ", // nf-oct-screen_full
                                false => "",
                            },
                            R::Accent,
                        )
                        .colored(&tab.name, R::Primary)
                        .space();
                    // .colored("[", R::Accent)
                    // .add(format_progress(&tab.sequence, &active.combo));
                    // .colored("]", R::Accent);

                    let tab_text = match (active.mode, tab.panes.len()) {
                        (VisualMode::ShowOnlyTabs, 1) | (VisualMode::ShowTabsAndPanes, 1) => {
                            let trimmed = trim_mid(&tab.panes[0].name, 24, 6);
                            tab_text.colored("| ", R::Highlight).text(trimmed)
                        }
                        (VisualMode::ShowOnlyTabs, _) => tab_text
                            .text("(")
                            .colored(tab.panes.len().to_string(), R::Highlight)
                            .text(" panes)"),
                        _ => tab_text,
                    };

                    print_text_with_coordinates(
                        match tab.address == active.cursor {
                            true => tab_text.opaque().build(),
                            false => tab_text.build(),
                        },
                        0,
                        row_pos,
                        None,
                        None,
                    );

                    if let VisualMode::ShowTabsAndPanes = active.mode {
                        row_pos += 1;

                        if tab.panes.len() >= 2 {
                            for pane in tab.panes.iter() {
                                let focused = pane.is_focused;
                                let pane_text = TextBuilder::start()
                                    .add(match &pane.sequence {
                                        Some(sequence) => format_progress(sequence, &active.combo),
                                        None => TextBuilder::start(),
                                    })
                                    .colored(
                                        match (
                                            focused,
                                            pane.is_floating,
                                            tab.are_floating_panes_visible,
                                        ) {
                                            (true, true, true) | (true, false, false) => " * ", //" \u{f444} ",
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
                                    .colored(trim_mid(&pane.name, 30, 7), R::Default)
                                    .space();
                                // .colored("]", R::Accent);

                                print_text_with_coordinates(
                                    match pane.address == active.cursor {
                                        true => pane_text.opaque().build(),
                                        false => pane_text.build(),
                                    },
                                    2,
                                    row_pos,
                                    None,
                                    None,
                                );
                                row_pos += 1;
                            }
                        }
                    }

                    row_pos += 1;
                }

                let sym = &active.key_symbols;

                let fmt = |s| format!("{}", s);
                let legend = TextBuilder::start()
                    .text("Type ")
                    .colored("letters", R::HotKey)
                    .text(" to jump, ")
                    .colored(fmt(&sym.esc), R::HotKey)
                    .text(" close, ")
                    .colored(format!("{}{}", sym.up, sym.down), R::HotKey)
                    // .text(" or ")
                    // .colored(format!("{}", sym.tab), R::HotKey)
                    .text(" select, ")
                    .colored(fmt(&sym.enter), R::HotKey)
                    .text(" jump to selection,")
                    .colored(" Space", R::HotKey)
                    .text(match active.mode {
                        VisualMode::ShowOnlyTabs => " show panes",
                        VisualMode::ShowTabsAndPanes => " hide panes",
                    });
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
                address: Address::Tab(0),
                current: false,
                are_floating_panes_visible: true,
                is_fullscreen_active: false,
                sequence: One('a'),
                panes: vec![
                    Pane {
                        name: "Pane 1".to_string(),
                        id: PaneId::Terminal(1),
                        address: Address::Pane(0, PaneId::Terminal(1)),
                        sequence: Some(Two('b', 'c')),
                        is_focused: true,
                        is_floating: true,
                    },
                    Pane {
                        name: "Pane 2".to_string(),
                        id: PaneId::Terminal(2),
                        address: Address::Pane(0, PaneId::Terminal(2)),
                        sequence: Some(One('d')),
                        is_floating: false,
                        is_focused: false,
                    },
                ],
            },
            Tab {
                current: true,
                address: Address::Tab(1),
                are_floating_panes_visible: false,
                is_fullscreen_active: false,
                name: "Tab 2".to_string(),
                sequence: Two('e', 'f'),
                panes: vec![
                    Pane {
                        is_floating: false,
                        name: "Pane 3".to_string(),
                        id: PaneId::Terminal(3),
                        address: Address::Pane(1, PaneId::Terminal(3)),
                        sequence: Some(One('g')),
                        is_focused: false,
                    },
                    Pane {
                        is_floating: false,
                        name: "Pane 4".to_string(),
                        id: PaneId::Terminal(4),
                        address: Address::Pane(1, PaneId::Terminal(4)),
                        sequence: Some(Two('h', 'i')),
                        is_focused: false,
                    },
                ],
            },
        ];

        assert!(matches!(
            check_overall_tree(tabs.iter(), "a"),
            TreeMatch::Match(Address::Tab(0))
        ));

        assert!(matches!(
            check_overall_tree(tabs.iter(), "bc"),
            TreeMatch::Match(Address::Pane(0, PaneId::Terminal(1)))
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
            TreeMatch::Match(Address::Tab(1))
        ));

        assert!(matches!(
            check_overall_tree(tabs.iter(), "hi"),
            TreeMatch::Match(Address::Pane(1, PaneId::Terminal(4)))
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

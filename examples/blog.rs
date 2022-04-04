use std::fmt::{self, Write};

use clap::Parser;
use idm;
use indexmap::IndexMap;
use rouille::router;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
struct BlogData {
    title: String,
    author: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct Entry {
    #[serde(with = "date_format")]
    published: chrono::NaiveDate,
    #[serde(default)]
    tags: Vec<String>,
    #[serde(default)]
    slug: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct Outline(Vec<Item>);

impl std::ops::Deref for Outline {
    type Target = Vec<Item>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Outline items with optional attributes.
///
/// The attributes are used to tag the headline as a HTML element
#[derive(Debug, Serialize, Deserialize)]
struct Item(String, (IndexMap<String, String>, Outline));

impl fmt::Display for Outline {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<p>")?;

        for i in &self.0 {
            let attrs = &i.1 .0;
            if i.0.is_empty() {
                // Blank line, make a paragraph break.
                // Blank lines should parse so that they never have child
                // lines in the outline.
                writeln!(f, "</p><p>")?;
                continue;
            }

            // Hyperlinks
            if attrs.contains_key("href") {
                writeln!(f, "<a href='{}'>{}</a>", attrs["href"], i.0)?;
                continue;
            }

            // Images
            if attrs.contains_key("img") {
                writeln!(
                    f,
                    "<p><img src='{}' alt='{}'/></p>",
                    attrs["img"], i.0
                )?;
                continue;
            }

            // Regular text.
            writeln!(f, "{}", i.0)?;
            // If there's nested contents, print those too.
            if !i.1 .1.is_empty() {
                writeln!(f, "{}", i.1 .1)?;
            }
        }

        write!(f, "</p>")
    }
}

pub fn main() {
    #[derive(Parser, Debug)]
    struct Args {
        #[clap(long, default_value_t = 8080)]
        port: u16,

        #[clap(long)]
        path: Option<String>,
    }

    let args = Args::parse();

    // Load and parse blog.

    let blog_file = args.path.map(|p| {
        std::fs::read_to_string(p).expect("Failed to load blog at given path")
    });

    let mut blog = idm::from_str::<(
        BlogData,
        IndexMap<String, (Entry, Outline)>,
    )>(if blog_file.is_some() {
        blog_file.as_ref().unwrap()
    } else {
        // Example blog for lazy people.
        include_str!("blog.idm")
    })
    .expect("Failed to parse blog");

    // Generate missing slugs.
    for (title, (entry, _)) in blog.1.iter_mut() {
        if entry.slug.is_empty() {
            entry.slug = slug::slugify(title);
        }
    }

    // TODO: Check for malformed or repeating slugs

    // Run blog server.

    let url = format!("localhost:{}", args.port);
    println!("Starting server at http://{}", url);

    rouille::start_server(url, move |request| {
        router!(request,
            (GET) (/) => {
                // XXX: Might want to cache this...
                let mut buf = String::new();

                write!(&mut buf, "<h1>{}</h1>", blog.0.title).unwrap();

                for (title, (entry, _)) in &blog.1 {
                    write!(&mut buf, "<p><a href='/{}'>{}</a> | {}</p>",
                        entry.slug, title, entry.published).unwrap();
                }
                rouille::Response::html(buf)
            },

            (GET) (/{id: String}) => {
                if let Some((title, (entry, outline))) = blog.1.iter().find(|(_, (entry, _))| entry.slug == id) {
                    let mut buf = String::new();
                    writeln!(&mut buf, "<p><a href='/'>{}</a></p>", blog.0.title).unwrap();
                    writeln!(&mut buf, "<h1>{}</h1>", title).unwrap();
                    write!(&mut buf, "<em>Tags: ").unwrap();
                    for t in &entry.tags {
                        write!(&mut buf, "<a href='/Tagged/{}'>{}</a> ", t, t).unwrap();
                    }
                    writeln!(&mut buf, " | {} | Written by {}</em>", entry.published, blog.0.author).unwrap();

                    writeln!(&mut buf, "{}", outline).unwrap();

                    rouille::Response::html(buf)
                } else {
                    rouille::Response::empty_404()
                }
            },

            (GET) (/Tagged/{_id: String}) => {
                // TODO: Support tag listings
                rouille::Response::text("TODO!")
            },
            // TODO: Atom feed

            _ => rouille::Response::empty_404()
        )
    });
}

mod date_format {
    use chrono::NaiveDate;
    use serde::{self, Deserialize, Deserializer, Serializer};

    const FORMAT: &'static str = "%Y-%m-%d";

    pub fn serialize<S>(
        date: &NaiveDate,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let s = format!("{}", date.format(FORMAT));
        serializer.serialize_str(&s)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<NaiveDate, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        NaiveDate::parse_from_str(&s, FORMAT).map_err(serde::de::Error::custom)
    }
}

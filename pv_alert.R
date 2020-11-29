library(tidyverse)
library(lubridate)
library(glue)

# devtools::install_github("Tatvic/RGoogleAnalytics")
library(RGoogleAnalytics)

ga_id_strony <- "ga:wstaw_swoje"
gmail_password <- "wstaw_swoje"
gmail_mail <- "wstaw_swoje@gmail.com"
client_secret <- "wstaw_swoje"
client_id <- "wstaw_swoje"

ile_godzin_wstecz <- 36


oauth_token <- Auth(client.id = client_id, client.secret = client_secret)


ValidateToken(oauth_token)

# Dimensions & Metrics
# https://developers.google.com/analytics/devguides/reporting/core/dimsmets

# dane do dzisiaj, z ostatniego tygodnia
query.list <- Init(
    start.date = as.character(Sys.Date() - 90),
    end.date = as.character(Sys.Date()),
    dimensions = "ga:date,ga:hour,ga:sourceMedium", # up to 7
    metrics = "ga:pageviews",
    max.results = 20000,
    table.id = ga_id_strony
)

ga.query <- QueryBuilder(query.list)
ga.data.org <- GetReportData(ga.query, oauth_token)

# daty w normalnym stylu :)
ga.data <- ga.data.org %>%
    mutate(date = ymd(date)) %>%
    mutate(datetime = with_tz(
        make_datetime(
            year(date),
            month(date),
            day(date),
            as.numeric(hour)
        ) - hours(1),
        "Europe/Warsaw"
    )) %>%
    group_by(datetime) %>%
    summarise(pageviews = sum(pageviews)) %>%
    ungroup() %>%
    filter(datetime <= floor_date(now(), unit = "hours"))

ga.data.org <- ga.data.org %>%
    mutate(date = ymd(date)) %>%
    mutate(datetime = with_tz(
        make_datetime(
            year(date),
            month(date),
            day(date),
            as.numeric(hour)
        ) - hours(1),
        "Europe/Warsaw"
    )) %>%
    filter(datetime <= floor_date(now(), unit = "hours"))



m <- mean(ga.data$pageviews)
s <- sd(ga.data$pageviews)

# usunięcie extremów m+3sigma
data_part <- ga.data %>% filter(pageviews <= m + 2 * s)

# liczymy średnie i sd na danych bez ekstremów
nm <- round(mean(data_part$pageviews))
ns <- round(sd(data_part$pageviews))


h_means <- ga.data %>%
    mutate(h = hour(datetime)) %>%
    group_by(h) %>%
    summarise(pv = mean(pageviews)) %>%
    ungroup()


h_means <- tibble(datetime = seq(floor_date(now() - hours(ile_godzin_wstecz), unit = "hour"),
    floor_date(now(), unit = "hour"),
    by = "1 hour"
)) %>%
    mutate(h = hour(datetime)) %>%
    left_join(h_means, by = "h")


current_pv <- ga.data %>%
    filter(datetime == max(datetime)) %>%
    pull(pageviews)
mean_h_mean <- h_means %>%
    filter(datetime == max(datetime)) %>%
    pull(pv) %>%
    round()

# pv_limit <- round(nm+ns)
pv_limit <- round(nm + 2 * ns)

plot_data <- ga.data %>% filter(datetime >= now() - hours(ile_godzin_wstecz))
plot_date_min <- h_means %>%
    pull(datetime) %>%
    min()
plot_date_min <- plot_date_min - dhours(1)

plot <- ggplot(plot_data, aes(datetime, pageviews)) +
    geom_hline(yintercept = c(nm, nm + ns, nm + 2 * ns, nm + 3 * ns), color = "red") +
    geom_hline(yintercept = pv_limit, color = "blue") +
    annotate("text",
        x = rep(plot_date_min + 3600, 4),
        y = c(nm, nm + ns, nm + 2 * ns, nm + 3 * ns),
        label = expression(mu, mu + sigma, mu + 2 * sigma, mu + 3 * sigma),
        vjust = -0.2, hjust = "right"
    ) +
    geom_col(data = h_means, aes(datetime, pv), fill = "gray60") +
    geom_col(fill = "lightgreen", color = "gray60", alpha = 0.7, size = 1) +
    geom_text(aes(label = pageviews), size = 3, vjust = -1.05) +
    scale_x_datetime(date_labels = "%d/%m\n%H:%M", date_breaks = "3 hours") +
    scale_y_continuous(expand = c(0.25, 0)) +
    labs(
        title = glue("PV za ostatnie {ile_godzin_wstecz} godzin"),
        subtitle = format(now(), "Wygenerowano @ %Y-%m-%d, %H:%M"),
        x = "", y = ""
    ) +
    theme_minimal()

ggsave("wykres.png", plot, width = 10, height = 5, units = "in", dpi = 90)


if (current_pv >= pv_limit) {

    # najpopularniejsze źródła z ostatniej godziny
    tab_str <- ga.data.org %>%
        filter(datetime == max(datetime)) %>%
        select(sourceMedium, pageviews) %>%
        arrange(desc(pageviews)) %>%
        mutate(procent = round(100 * pageviews / sum(pageviews), 1)) %>%
        knitr::kable("html") %>%
        as.character()

    mail_html_body <- glue("<h3>Hejka!</h3>

<p>Zanotowałem większy niż zwykle ruch na stronie.</p>

<p>W ostatniej godzinie zanotowano <strong>{current_pv}</strong> PV.<br />
Średnio o tej godzinie notowane jest <strong>{mean_h_mean}</strong> <sup>PV</sup>/<sub>h</sub>.
Średnia z ostatnich 90 dni to <strong>{nm}</strong> <sup>PV</sup>/<sub>h</sub>.<br />
Alert wyzwala się przy średnia + 3 sigma, czyli <strong>{pv_limit}</strong> <sup>PV</sup>/<sub>h</sub>.</p>

<p>Najpopularniejsze źródła ruchu z ostatniej godziny:</p>
{tab_str}

<p><strong>Miłego dnia!<strong></p>")

    mailR::send.mail(
        from = gmail_mail,
        to = gmail_mail,
        subject = "Większy niż zwykle ruch na stronie!",
        body = mail_html_body,
        encoding = "utf-8",
        html = TRUE,
        smtp = list(
            host.name = "smtp.gmail.com",
            port = 465,
            user.name = gmail_mail,
            passwd = gmail_password,
            ssl = TRUE
        ),
        authenticate = TRUE,
        send = TRUE,
        attach.files = "wykres.png",
        file.names = "Wykres PV z ostatnich 90 dni"
    )
} else {
    print("W normie")
}
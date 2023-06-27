-module(web_crawler).
-export([start/1, crawl/2]).

% start/1 is the entry point function
start(Url) ->
    spawn(fun() -> crawl(Url, []) end).

% crawl/2 is the recursive function that performs the crawling
crawl(Url, Visited) ->
    case lists:member(Url, Visited) of
        true ->
            io:format("Skipping already visited URL: ~s~n", [Url]),
            ok;
        false ->
            case httpc:request(get, {Url, []}, [], []) of
                {ok, {{_, 200, _}, _, Body}} ->
                    io:format("Retrieved content from URL: ~s~n", [Url]),
                    process_page(Body),
                    Visited2 = [Url | Visited],
                    extract_links(Body, Url, Visited2);
                {ok, {{_, Code, _}, _, _}} ->
                    io:format("Failed to retrieve URL: ~s (Code: ~w)~n", [Url, Code]),
                    ok;
                {error, Reason} ->
                    io:format("Error retrieving URL: ~s (Reason: ~p)~n", [Url, Reason]),
                    ok
            end
    end.

% process_page/1 can be customized to handle the retrieved page content
process_page(Body) ->
    % Placeholder function - do something with the page content
    io:format("Processing page content: ~s~n", [Body]).

% extract_links/3 extracts links from the page and initiates crawling for each link
extract_links(Body, BaseUrl, Visited) ->
    Links = extract_links_from_html(Body),
    spawn_link(fun() -> crawl_links(Links, BaseUrl, Visited) end).

% crawl_links/3 crawls the extracted links in parallel
crawl_links([], _, _) ->
    ok;
crawl_links([Link | RestLinks], BaseUrl, Visited) ->
    FullUrl = resolve_url(Link, BaseUrl),
    spawn_link(fun() -> crawl(FullUrl, Visited) end),
    crawl_links(RestLinks, BaseUrl, Visited).

% extract_links_from_html/1 can be customized to extract links from the HTML content
extract_links_from_html(Html) ->
    % Placeholder function - extract links from the HTML content
    % You can use regular expressions or HTML parsing libraries to extract links
    ["/page1", "/page2", "/page3"].

% resolve_url/2 resolves relative URLs with respect to the base URL
resolve_url(Link, BaseUrl) ->
    % Placeholder function - resolve relative URLs
    BaseUrl ++ Link.

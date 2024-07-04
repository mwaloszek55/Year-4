package com.example.multi_activity_app;

import androidx.appcompat.app.AppCompatActivity;
import android.os.Bundle;
import android.webkit.WebView;
import android.webkit.WebViewClient;

import com.example.multi_activity_app.R;

public class WebInfoActivity extends AppCompatActivity {
    WebView webView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_web_info);

        webView = findViewById(R.id.webView);

        // Enable JavaScript (if the web page uses JavaScript)
        webView.getSettings().setJavaScriptEnabled(true);

        // Provide a WebViewClient for your WebView
        webView.setWebViewClient(new WebViewClient() {
            @Override
            public boolean shouldOverrideUrlLoading(WebView view, String url) {
                view.loadUrl(url);
                return true;
            }
        });

        // Load the URL passed from the intent
        String link = getIntent().getStringExtra("link");
        if (link != null) {
            webView.loadUrl(link);
        }
    }

    //
    @Override
    public void onBackPressed() {
        if (webView.canGoBack()) {
            webView.goBack();
        } else {
            super.onBackPressed();
        }
    }
}
